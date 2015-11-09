#!/usr/bin/env perl

use strict;
use warnings;
use utf8;

use Test::More 1;
use Test::Exception;
use Util::Underscore v1.3.0;

use constant {
    EXCEPTION => 'MarpaX::Grammar::Preprocessor::Exception',
    PARSE_EXCEPTION => 'MarpaX::Grammar::Preprocessor::ParseException',
};

use_ok EXCEPTION;

my $__subtest = sub {
    my ($name, $body) = @_;
    our $__test_prefix;
    my $full_name = join ': ', grep defined, $__test_prefix, $name;
    local $__test_prefix = $full_name;
    return subtest $full_name, $body;
};

sub describe {
    my ($name, $body) = @_;
    return $__subtest->("describe $name", $body);
}

sub it {
    my ($name, $body) = @_;
    return $__subtest->("it $name", $body);
}

sub case {
    my ($name, $body) = @_;
    return $__subtest->("case $name", $body);
}

sub f1 { my $f = shift; $f->(@_) }
sub f2 { my $f = shift; $f->(@_) }

sub get_err_and_stack_trace {
    my $maker = sub { my $class = shift; $class->new(@_) };
    my $err = $maker->(@_); my $trace = $err->stack_trace_class->new;
    return $err, $trace;
};

describe EXCEPTION() => sub {
    describe new_f => sub {
        it 'can take a format' => sub {
            my $e = EXCEPTION->new_f('format %s - %d', ["foo", 42]);
            is $e->message, 'format foo - 42';
        };

        it 'can take further parameters' => sub {
            my $e = EXCEPTION->new_f('ignore', [], message => 'real message');
            is $e->message, 'real message';
        };
    };

    describe as_string => sub {
        it 'has a message and a stack trace' => sub {
            my ($err, $expected_trace) = get_err_and_stack_trace(
                EXCEPTION,
                message => 'the message',
            );
            is $err->as_string, "the message\n\n$expected_trace";
            is "$err", "the message\n\n$expected_trace";
        };

        it 'takes a callback for extra info' => sub {
            my ($err, $expected_trace) = get_err_and_stack_trace(
                EXCEPTION,
                message => 'the message',
            );
            is $err->as_string(sub { "extra info\n" }), "the message\n\nextra info\n\n$expected_trace";
        };
    };

    describe stack_trace => sub {
        dies_ok { f1 \&f2, sub { EXCEPTION->throw(message => 'the message') } };
        my $stack_trace = $@->stack_trace;
        is $stack_trace->next_frame->subroutine, 'Throwable::throw';
        is $stack_trace->next_frame->subroutine, 'main::__ANON__';
        is $stack_trace->next_frame->subroutine, 'main::f2';
        is $stack_trace->next_frame->subroutine, 'main::f1';
    };
};

describe PARSE_EXCEPTION() => sub {
    describe as_string => sub {
        it 'dumps the context' => sub {
            my ($err, $stack_trace) = get_err_and_stack_trace(
                PARSE_EXCEPTION,
                message => "the message",
                context_seen => "this should\xA0come before",
                context_next => "this \$should come after",
                line_contents => 'something happens on this line',
                line_number => 42,
                column => 4,
            );
            my $expected
                = qq(the message\n)
                . qq(\n)
                . qq(after: "this should\\xA0come before"\n)
                . qq(before: "this \\\$should come after"\n)
                . qq(on line 42:\n)
                . qq(something happens on this line\n)
                . qq(    ^--\n)
                . qq(\n)
                . $stack_trace;

            is $err->as_string, $expected;
        };

        it 'takes a callback for extra info' => sub {
            my ($err, $stack_trace) = get_err_and_stack_trace(
                PARSE_EXCEPTION,
                message => "the message",
                context_seen => "this should\xA0come before",
                context_next => "this \$should come after",
                line_contents => 'something happens on this line',
                line_number => 42,
                column => 4,
            );
            my $expected
                = qq(the message\n)
                . qq(\n)
                . qq(after: "this should\\xA0come before"\n)
                . qq(before: "this \\\$should come after"\n)
                . qq(on line 42:\n)
                . qq(something happens on this line\n)
                . qq(    ^--\n)
                . qq(\n)
                . qq(extra info\n)
                . qq(\n)
                . $stack_trace;

            is $err->as_string(sub { "extra info\n" }), $expected;
        };

        it 'omits line contents when undef' => sub {
            my ($err, $stack_trace) = get_err_and_stack_trace(
                PARSE_EXCEPTION,
                message => "the message",
                context_seen => "this should\xA0come before",
                context_next => "this \$should come after",
                line_contents => undef,
                line_number => undef,
                column => undef,
            );
            my $expected
                = qq(the message\n)
                . qq(\n)
                . qq(after: "this should\\xA0come before"\n)
                . qq(before: "this \\\$should come after"\n)
                . qq(\n)
                . $stack_trace;

            is $err->as_string, $expected;
        };

        it 'can deal with undef context' => sub {
            my ($err, $stack_trace) = get_err_and_stack_trace(
                PARSE_EXCEPTION,
                message => "the message",
                context_seen => undef,
                context_next => undef,
                line_contents => undef,
                line_number => undef,
                column => undef,
            );
            my $expected
                = qq(the message\n)
                . qq(\n)
                . qq(after: undef\n)
                . qq(before: undef\n)
                . qq(\n)
                . $stack_trace;

            is $err->as_string, $expected;
            is "$err", $expected;
        };
    };

    describe new_from_source_f => sub {
        it 'returns a blank object when source ref is undef' => sub {
            my $err = PARSE_EXCEPTION->new_from_source_f(
                undef,
                "message %s",
                "format",
            );
            my $expected
                = qq(message format\n)
                . qq(\n)
                . qq(after: undef\n)
                . qq(before: undef\n);
            like $err, qr/\A\Q$expected\E\nTrace begun at .* line \d+\n/
        };

        it 'extracts context' => sub {
            my $source = "foobar";
            pos($source) = 3;
            my $err = PARSE_EXCEPTION->new_from_source_f(\$source, "message");
            is $err->context_seen, "foo";
            is $err->context_next, "bar";
        };

        it 'extracts context up to 20 chars' => sub {
            my $run = "1234567890";
            is length $run, 10;
            my $source = "$run$run$run$run";
            is length $source, 40;
            pos($source) = 20;
            my $err = PARSE_EXCEPTION->new_from_source_f(\$source, "message");
            is $err->context_seen, "$run$run";
            is $err->context_next, "$run$run";
        };

        it 'cuts off context after 20 chars' => sub {
            my $run = "1234567890";
            is length $run, 10;
            my $source = "x$run$run$run${run}x";
            is length $source, 42;
            pos($source) = 21;
            my $err = PARSE_EXCEPTION->new_from_source_f(\$source, "message");
            is $err->context_seen, "$run$run";
            is $err->context_next, "$run$run";
        };

        it 'finds the current line' => sub {
            my @line_extraction_test_cases = (
                ['single line' => "foo", "bar", 1, 3, "foobar"],

                ['empty line on first line' => "", "\nnux", 1, 0, ''],
                ['empty line' => "qux\n", "\nnux", 2, 0, ''],
                ['empty line on last line' => "qux\n", "", 2, 0, ''],

                ['first line' => "foo", "bar\nnux", 1, 3, "foobar"],
                ['last line' => "qux\nfoo", "bar", 2, 3, "foobar"],
                ['middle line' => "qux\nfoo", "bar\nnux", 2, 3, "foobar"],

                ['first column of first line' => "", "foobar\nqux", 1, 0, "foobar"],
                ['first column' => "qux\n", "foobar\nnux", 2, 0, "foobar"],

                ['last column of last line' => "qux\nfoobar", "", 2, 6, "foobar"],
                ['last column' => "qux\nfooba", "r\nnux", 2, 5, "foobar"],

                ['line break on first line' => "foobar", "\nnux", 1, 6, "foobar"],
                ['line break on second line' => "qux\nfoobar", "\nnux", 2, 6, "foobar"],
                ['line break' => "quxqux\nqux\nfoobar", "\nnux", 3, 6, "foobar"],
            );

            for my $case_spec (@line_extraction_test_cases) {
                my ($name, $before, $after, $expected_line_number, $expected_col, $expected_line) = @$case_spec;
                case $name => sub {
                    my $source = $before . $after;
                    pos($source) = length $before;
                    my $err = PARSE_EXCEPTION->new_from_source_f(\$source, "message");
                    is $err->column, $expected_col, 'column';
                    is $err->line_number, $expected_line_number, 'line number';
                    is $err->line_contents, $expected_line, 'line contents';
                };
            }
        };

        # TODO
    };
};

done_testing;
