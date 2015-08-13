package MarpaX::R2::GrammarPreprocessor;

use strict;
use warnings;
use utf8;

use version 0.77; our $VERSION = qv('v0.0_1');

use Util::Underscore v1.3.0;
use Lexical::Accessor 0.008;

use Moo 2;

lexical_has namespace   => (accessor => \my $NAMESPACE);
lexical_has docs        => (accessor => \my $DOCS);
lexical_has source_ref  => (accessor => \my $SOURCE_REF);
lexical_has buffers     => (accessor => \my $BUFFERS);

# token types
use constant TOKEN_TYPES => {
    IDENT   => 1,
    LITERAL => 2,
    OP      => 3,
    CLOSE   => 4,
};
use constant TOKEN_TYPES; # defines IDENT => 1, etc.
use constant TOKEN_NAMES => do {
    my %reversed;
    while (my ($name, $id) = each %{TOKEN_TYPES()}) {
        die sprintf "redefined id %d from %s to %s", $id, $reversed{$id}, $name
            if exists $reversed{$id};
        $reversed{$id} = $name;
    }
    \%reversed;
};

use constant {
    NAMESPACE_SEPARATOR => '__',
    SLIF_PRELUDE =>
        'inaccessible is fatal by default ' .
        ':default ::= action => ::first ' .
        'lexeme default = latm => 1 ',
};

sub BUILD {
    my ($self) = @_;
    $self->$NAMESPACE(undef);
    $self->$DOCS({});
    $self->$BUFFERS([SLIF_PRELUDE]); # prime the first buffer
    return;
}

sub preprocess {
    my ($class, $source) = @_;

    $source = do { local $/; <$source> } if _::is_open $source;
    pos($source) = 0;

    my $self = $class->_get_fresh_instance(\$source);

    $self->_pump;

    return $self;
}

# DO NOT USE – internal method
#
# create and initialize a new instance
#
# Unfortunately, there's no good way to initialize lexical accessors from a
# constructor, so we wrap the ctor instead.
sub _get_fresh_instance {
    my ($class, $source_ref) = @_;

    my $self = $class->new;

    $self->$SOURCE_REF($source_ref);

    return $self;
}

# DO NOT USE – internal method
#
# poll for tokens until source is exhausted
#
# This pumps the whole input through the preprocessor.
sub _pump {
    my ($self) = @_;

    # pump the whole source through the preprocessor
    $self->buffer_push;
    while (my ($type, $value) = $self->next_token) {
        $self->buffer_write($value);
    }
    $self->buffer_join;

    return;
}

sub buffer_push {
    my ($self, $init) = @_;
    push @{ $self->$BUFFERS }, $init // '; ';
    return;
}

sub buffer_join {
    my ($self) = @_;
    my $last_buffer = pop @{ $self->$BUFFERS };
    $self->$BUFFERS->[-1] .= $last_buffer;
    return;
}

sub buffer_write {
    my ($self, @things) = @_;
    $self->$BUFFERS->[-2] .= join '', @things;
    return;
}

sub slif_source {
    my ($self) = @_;
    my $buffers = $self->$BUFFERS;

    die "Inline rules were not closed" if @$buffers != 1;

    return $buffers->[0];
}

sub docs {
    my ($self) = @_;
    return $self->$DOCS;
}

sub expect {
    my ($self, @expected) = @_;
    my ($type, $value) = $self->next_token
        or die sprintf "expected {%s} but reached end of input", (join ', ', @{TOKEN_NAMES()}{@expected});
    $type == $_ and return $value for @expected;
    die sprintf "expected {%s} not %s", (join ', ', @{TOKEN_NAMES()}{@expected}), TOKEN_NAMES->{$type};
}

sub next_token {
    my ($self) = @_;
    local *_ = $self->$SOURCE_REF;
    TRY: while (pos() < length) {
        if (m/\G( \s+ )/xgc) {
            $self->buffer_write($1);
            next TRY;
        }
        if (m/\G( [#] [^\n]* )/xgc) {
            $self->buffer_write($1);
            next TRY;
        }
        return IDENT, $1 if m/\G( [\w]+ )/xgc;
        if (m/\G [%]/xgc) {
            my $ns = $self->$NAMESPACE;
            die "No namespace was set" if not defined $ns;
            return IDENT, $ns . $self->NAMESPACE_SEPARATOR . $1 if m/\G ([\w]+) /xgc;
            return IDENT, $ns;
        }
        return LITERAL, $1 if m/\G( ['] [^']+ ['] (?: [:]i )? )/xgc;
        return LITERAL, $1 if m/\G( [\[] (?: [^\]\\]++ | [\\]. )+ [\]] )/xgc;
        if (m/\G [\\] (\w+)/xgc) {
            my $cmd = $1;
            my $command = $self->can("command_$cmd")
                or die "Unknown command $cmd, could not find command_$cmd method";
            return $command->($self);
        }
        if (m/\G [{]/xgc) {
            $self->buffer_push;
            my $name = $self->expect(IDENT);
            $self->buffer_write($name);
            while (my ($type, $value) = $self->next_token) {
                last if $type == CLOSE;
                $self->buffer_write($value);
            }
            $self->buffer_join;
            return IDENT, $name;
        }
        return CLOSE,   undef   if m/\G [\}]/xgc;
        return OP,      $1      if m/\G( [^\s\#\w\%\'\[\\]+ )/xgc;
        die "Unknown grammar str '" . (substr $_, pos, 20) . "...'";
    }
    return;
}

# DO NOT USE – only for testing
# Subject to change without any notice
#
# Break encapsulation so that we can write white-box tests
sub _test_accessors {
    my ($self, $name) = @_;
    return +{
        namespace   => $NAMESPACE,
        docs        => $DOCS,
        source_ref  => $SOURCE_REF,
        buffers     => $BUFFERS,
    }->{$name};
}

sub command_array { return OP, 'action => ::array' }

sub command_null { return OP, 'action => ::undef' }

sub command_group { return OP, 'assoc => group' }

sub command_left { return OP, 'assoc => left' }

sub command_right { return OP, 'assoc => right' }

sub command_lax { return OP, 'proper => 0' }

sub command_do {
    m/\G \s+ (\w+) /xgc
        or die "Expected action name";
    return OP, "action => do_$1";
}

sub command_sep {
    my ($self) = @_;
    my $sep = $self->expect(IDENT, LITERAL);
    return OP, "separator => $sep";
}

sub command_keyword {
    my ($self) = @_;
    my $name = $self->expect(IDENT);
    $self->buffer_write(":lexeme ~ $name priority => 1;");
    return IDENT, $name;
}

sub command_namespace {
    my ($self) = @_;
    my $name = $self->expect(IDENT);
    $self->$NAMESPACE($name);
    return IDENT, $name;
}

sub command_doc {
    my ($self) = @_;
    my $docs;
    if (m/\G \s* hide/xgc) {
        $docs = undef
    }
    elsif (m/\G \s* ["]["]["]\ ([^\n]* (?: \n | \z ))/xgc) {
        do {
            $docs .= $1
        } while (m/\G \s* ["]["]["]\ ([^\n]* (?: \n | \z ))/xgc);
        $docs =~ s/\s*\z//;
    }
    else {
        my $next_source = substr $_, pos, 20;
        die "Expected docstring near ", _::pp $next_source, "...";
    }

    my $symbol = $self->expect(IDENT);
    die "Symbol $symbol already documented" if exists $self->$DOCS->{$symbol};
    $self->$DOCS->{$symbol} = $docs;
    return IDENT, $symbol;
}

1;

__END__

=pod

=encoding utf8

=head1 NAME

MarpaX::R2::GrammarPreprocessor - Shortcuts for Marpa::R2 SLIF grammars

=head1 VERSION

v0.0_1

=head1 SYNOPSIS

    use MarpaX::R2::GrammarPreprocessor;
    use Marpa::R2;

    my $preprocessed = MarpaX::R2::GrammarPreprocessor->preprocess(\*DATA)
    my $grammar = Marpa::R2::Scanless::G->new({ source => \$preprocessed->slif_source });

    __DATA__
    # Everything that's legal in the SLIF DSL is also legal here

    # We automatically get this prelude:
    #   inaccessible is fatal by default
    #   :default ::= action => ::first
    #   lexeme default = latm => 1

    # A namespace allows us to gensym names for %helper rules
    \namespace Foo ::= %BAR | %Baz
    %BAR ~ 'bar'        # really Foo__BAR
    %Baz ::= %BAR %BAR  # really Foo__Baz

    # The next namespace is totally unrelated.
    \namespace
    Qux ::= %BAR
    %BAR ~ 'quxbar'     # really Qux__BAR

    # Associate a docstring with the next symbol.
    # Docstrings can span multiple lines, all beginning with a triple quote.
    \namespace
    \doc""" a list of values. Examples:
        """     []          (empty list)
        """     [1, 2, 3]   (list with three integers)
    List ::= (LEFT_BRACKET) %Items (RIGHT_BRACKET)
    %Items ::= Value* \sep COMMA  # \sep expands to "separator => "

    # Use { curly braces } to specify an inline rule.
    # Inline rules still need a name.
    # \array expands to "action => ::array"
    \namespace
    \doc""" a key-value dictionary
    Dict ::= (LEFT_BRACE) { %Items ::= %KVItem* \sep COMMA \array } (RIGHT_BRACE)
    %KVItem ::= Key (COLON) Value \array

    # Suppress documentation for any symbol.
    # Great for internal helper rules!
    \doc hide
    Shhh ~ 'no one can see me'

    # Easily link action rules with \do
    Action ::= Stations \do Action # expands to "action => do_Action"

=head1 DESCRIPTION

This module is a preprocessor for SLIF grammars.
Any valid SLIF grammar should be passed through with no modifications,
except for the prelude that is added.

This preprocessor is fairly restricted and mostly only does local,
token-based substitutions, similar to the C preprocessor.
The inline rule feature is more advanced,
but still operates on a source level.
Please keep this low-level approach in mind when using the module
– it does not build an AST for the SLIF, and does not use Marpa itself.

=head2 Commands

The most prominent feature are the commands.
These are introduced by a backslash,
and call back to Perl code which may do custom parsing
or use a simple token-based system to process the SLIF grammar.
Example: C<\lax \sep COMMA> is transformed to C<< proper => 0 separator => COMMA >>.

=head2 Namespaces

Frequently, we need simple helper rules
that are of no concern for the rest of the grammar.
This preprocessor can mark any identifier as the current C<\namespace>.
Any plain C<%> (percent symbol) is then used as a reference to the current namespace,
and any C<%name> (name prefixed with percent symbol) is prefixed with the current namespace.
This makes it easy to have quasi-private names without too much typing.

Example:

    \namespace
    Term ::= % (%PLUS) Factor
    %PLUS ~ '+'

Is transformed to:

    Term ::= Term (Term__PLUS) Factor
    Term__PLUS ~ '+'

The namespace separator is currently set to double underscores,
so you shouldn't use them in your identifiers
(see also the stabilit policy section).

=head2 Inline Rules

Many rules in a SLIF grammar are only used in one place,
and are due to SLIF restrictions.
E.g. a sequence rule must be a rule of its own.
This preprocessor allows you to specify rules inline at their point of usage.
Unfortunately, they still need a name.
The preprocessor will then replace the inline rule with its symbol,
and defer the defintion of the rule until a safe state is reached.

Example:

    \namespace
    List ::= ('[') { %Items ::= Value* \sep COMMA \array } (']')

Is transformed to:

    List ::= ('[') List__Items (']')
    List__Items ::= Value* separator => COMMA action => ::array


=head2 Docstrings

The C<\doc> commands lets you annotate a symbol with a docstring.
After processing, these docstrings are available as a name to docstring hash ref.
They can be used to build fairly sophisticated help systems that display relevant information on parse errors.

Example:

    \doc""" A list contains a sequence of zero or more values.
        """ It must start and end with square brackets,
        """ and contains comma-separated values. Example:
        """
        """    []      # an empty list
        """    [1]     # list with single element
        """    [1,]    # trailing comma is allowd
        """    [1,2,3] # list with three values
        """    [ 1 , 2 , 3 ]   # space within the array is ignored
    List ::= ...

This documentation could then be used to display a help message like this to the user:

    test.foo:42:4: error: no token found
        (1, 2, 3)
        ^
    at character '(' U+0028 LEFT PARENTHESIS

    expected:
      - List: A list contains a sequence of zero or more values.
        It must start and end with square brackets,
        and contains comma-separated values. Example:

            []      # an empty list
            [1]     # list with single element
            [1,]    # trailing comma is allowd
            [1,2,3] # list with three values
            [ 1 , 2 , 3 ]   # space within the array is ignored
      - Dict: A dict contains any number of key-value pairs
        ...
    ...

This module does not include such a help system!
However, you can see the test case C<t/json.t>
for a sample implementation of such “intelligent” error messages.
Run it as a script and pass it faulty input to see it in action.

=head2 Custom Commands

You can add custom commands by subclassing this module
and adding a C<command_FooBar> method for a C<\FooBar> command.
No additional registration has to be done.
Since this module uses the Moo object system, I recommend you use it as well.

When your command is found, the processor will invoke that method,
The SLIF source will be available in the C<$_> variable,
and C<pos()> will be set to the current position.
You can therefore use an C<m/\G ... /gc> style match to do your own parsing.
Alternatively, you can C<expect()> a certain token type,
or poll for the C<next_token()> regardless of type.
Please see their reference documentation for more details.

The command must return a list of two values: the token type and the token value.
Valid token types are the constants/methods
C<IDENT> which refers to any identifier or symbol that can be used on the left hand side of any rule defintion,
C<LITERAL> which can be a literal string or a character class,
C<CLOSE> which is a right curly brace, and
C<OP> which is anything else.
Pick an appropriate token type depending on how that value might be used.
E.g. if the return value is to be used on the right hand side of a rule,
it must be an C<IDENT> or C<LITERAL>.

You may also use the C<buffer_push()>, C<buffer_write()>, and C<buffer_join()> methods.
The buffer system manages any number of buffers.
One buffer is the currently selected buffer, another is the deferred buffer.
Only write to the current buffer if you know it is safe to do so
(e.g. if your command is supposed to be only used at the start of a rule).
Otherwise, select the deferred buffer with C<buffer_write()>.
Once finished, you will need to switch back with C<buffer_join()>.
The contents of the deferred buffer will be appended to the main buffer
once that buffer is complete and in a presumably syntactically safe state.

Actually, buffers are way less complicated than that – just read the source of this module.

=head1 METHODS

Some methods are only valid during processing,
other only for retrieval after processing.
Some methods may be overridable, and be marked as such.
Please see also the stability policy section.

=head1 STATUS OF THIS MODULE/STABILITY POLICY

TODO

=head1 BUGS

TODO

=head1 AUTHOR

TODO

=head1 COPYRIGHT AND LICENSE

TODO