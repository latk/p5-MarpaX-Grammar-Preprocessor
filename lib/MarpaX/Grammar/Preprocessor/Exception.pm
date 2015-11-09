use strict;
use warnings;
use utf8;

=pod

=encoding utf8

=head1 NAME

MarpaX::Grammar::Preprocessor::Exception - exception hierarchy

=head1 DESCRIPTION

All exceptions thrown by MarpaX::Grammar::Preprocessor are part of the exception hierarchy defined in this module.
All exceptions have overloaded stringification so that they require no special handling.
However, some types may offer additional fields that can be introspected.

=head1 INDEX

=begin :list

* L<Exception|/"Exception"> – exception base class

=for :list
* L<ParseException|/"ParseException"> – errors at a grammar source position

=end :list

=head2 COMMON ATTRIBUTES AND METHODS

All exceptions in this hierarchy consume the L<Throwable|Throwable> role,
and therefore offer the following methods:

=for :list
= C<previous_exception>
  accesses pre-existing exceptions.
= C<throw(CTOR_ARGS)>
  creates and throws a new exception instance.

They also consume the L<StackTrace::Auto|StackTrace::Auto> role,
which provides:

=for :list
= C<stack_trace>
  accesses the stack trace instance.
= C<stack_trace_class>
  is the class used to build the stack trace.
= C<stack_trace_args>
  are the arguments passed to the stack trace constructor.

=cut

package MarpaX::Grammar::Preprocessor::Exception;

=head1 Exception

Exception base class for MarpaX::Grammar::Preprocessor

This type is used when no other exception type is more appliccable.

Known subclasses:

=for :list
* L<ParseException|/"ParseException">

=head2 CONSTRUCTOR

    my $e = MarpaX::Grammar::Preprocessor::Exception->new(
        message => "something went wrong",
    );

Creates a new exception instance.

Known named arguments are:

=for :list
= message
  (required) a description of the error cause
= previous_exception
  (optional) a cause exception
= stack_trace
  (optional) the stack trace object for this error
= stack_trace_class
  (optional) the class to construct the stack trace of
= stack_trace_args
  (optional) arguments passed to the stack trace construct

=cut

use Moo;

use Util::Underscore;

use Throwable;
use StackTrace::Auto;
with 'Throwable';
with 'StackTrace::Auto';

use overload (
    '""' => 'as_string',
);

=head2 message

    my $str = $error->message

retrieve the error message

=cut

has message => (
    is => 'ro',
    required => 1,
);

=head2 new_f

    my $error = MarpaX::Grammar::Preprocessor::Exception->new_f(
        $sprintf_format,
        \@sprintf_format_args,
        %other_args,
    );

Creates a new exception instance with a sprintf message.

It is exactly equivalent to

    MarpaX::Grammar::Preprocessor::Exception->new(
        message => sprintf($sprintf_format, @sprintf_format_args),
        %other_args,
    )

=cut

sub new_f {
    my ($class, $format, $format_args, @args) = @_;
    return $class->new(
        message => (sprintf $format, @$format_args),
        @args,
    );
}

=head2 as_string

    $str = $error->as_string;
    $str = $error->as_string(\&extra_information);

Create a formatted string representation of the exception.

B<\&extra_information>
is a callback that takes the exception object as argument.
It must return a newline-terminated string
of any additional information that should be included in the error message,
which is convenient when extending this exception class.

=cut

sub as_string {
    my ($self, $inside) = @_;
    my $str = '';

    $str .= $self->message . "\n";

    if ($inside) {
        $str .= "\n";
        $str .= $self->$inside();
    }

    $str .= "\n";
    $str .= $self->stack_trace;

    return $str;
}

package MarpaX::Grammar::Preprocessor::ParseException;

=head1 ParseException

An exception associated with a grammar source position.

This is used for syntax errors,
or any other kind of error where knowing the location would aid debugging.

Superclass

=for :list
* L<Exception|/"Exception">

=cut

use Moo;
extends 'MarpaX::Grammar::Preprocessor::Exception';

=head2 column

The zero-based column on the current line where the error occurred.
Most systems use 1-based column counting, so you will have to adjust the value.

=head2 line_contents

The textual content of the current line, as a string.
It does not include the line terminator.

=head2 line_number

The 1-based line number, useful for quickly locating the error.

=head2 context_seen

The last couple of characters immediately before the error location.

=head2 context_next

The next couple of characters immediately after and including the error location.

=cut

has [qw/column line_contents line_number context_seen context_next/] => (
    is => 'ro',
    required => 1,
);

sub _MarpaX_Grammar_Preprocessor_ParseException_extract_line {
    my (undef, $source_ref, $pos) = @_;
    _::alias my $source = $$source_ref;

    my $line_contents;
    my $line_number = 0;
    my $line_start = 0;
    my $line_ending;
    pos($source) = 0;
    do {
        $line_start = pos($source);
        # uncoverable condition right
        $source =~ /\G(.*)(\R|\z)/gc
            or die "invalid state in new_from_source_f()\n";
        $line_contents = $1;
        $line_ending = $2;
        $line_number++;
    }
    until ( $pos < pos($source)
        or  length($source) <= $line_start
        or  (not length $line_ending)
        );
    pos($source) = $pos;

    my $line_column = $pos - $line_start;

    return ($line_number, $line_column, $line_contents);
}

=head2 new_from_source_f

    $err = MarpaX::Grammar::Preprocessor::ParseException->new_from_source_f(
        \$source,
        $sprintf_format,
        @sprintf_format_args,
    );

Create a ParseException at the match position of the source, with a formatted error message.

The error location is taken from C<pos($source)>,
which is correct when using C<m/.../gc> style regex matches for parsing
(MarpaX::Grammar::Preprocessor consistently does so).

B<$source>
is the parse input as a string.
Note that the method actually takes a string reference.
The C<pos()> of this string is used as the error position.

If an C<undef> reference is given,
then source information is omitted from the error
– the respective fields will be C<undef>.

B<$sprintf_format> and B<@sprintf_format_args>
are the same as in L<C<new_f()>|/"new_f">.

No other constructor arguments can be provided.

=cut

sub new_from_source_f {
    my ($class, $source_ref, $message_format, @message_args) = @_;
    if (not defined $source_ref) {
        return $class->new_f(
            $message_format,
            \@message_args,
            column => undef,
            line_contents => undef,
            line_number => undef,
            context_seen => undef,
            context_next => undef,
        );
    }

    _::alias my $source = $$source_ref;
    my $pos = pos($source);

    my $context_seen = substr $source, _::max($pos - 20, 0), _::min(20, $pos);
    my $context_next = substr $source, $pos, 20;

    my ($line_number, $line_column, $line_contents) = _::try {
        _MarpaX_Grammar_Preprocessor_ParseException_extract_line(undef, \$source, $pos);
    } _::catch {
        # uncoverable subroutine
        chomp;
        MarpaX::Grammar::Preprocessor::ParseException
            ->throw(
                message =>
                    "$_ while processing error:\n"
                    . (sprintf $message_format, @message_args),
                column => undef,
                line_contents => undef,
                line_number => undef,
                context_seen => $context_seen,
                context_next => $context_next,
            );
    };

    return $class->new(
        message => (sprintf $message_format, @message_args),
        column => $line_column,
        line_contents => $line_contents,
        line_number => $line_number,
        context_seen => $context_seen,
        context_next => $context_next,
    );
}

around as_string => sub {
    my ($orig, $self, $inside) = @_;
    return $self->$orig(sub {
        my ($self) = @_;
        my $str = '';

        $str .= "after: " . _::pp($self->context_seen) . "\n";
        $str .= "before: " . _::pp($self->context_next) . "\n";
        if (defined $self->line_contents) {
            $str .= "on line " . $self->line_number . ":\n";
            $str .= $self->line_contents . "\n";
            $str .= (q[ ] x $self->column) . '^--' . "\n";
        }

        if ($inside) {
            $str .= "\n";
            $str .= $self->$inside();
        }

        return $str;
    });
};

1;
