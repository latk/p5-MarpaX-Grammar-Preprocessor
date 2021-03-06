#!/usr/bin/env perl

# WARNING
#
# Welcome dear reader to my unit tests. Please note that these are white-box
# tests which muck around with the object's internals, These tests do not
# illustrate proper usage of the public interface. You might instead want to
# read the documentation, or the integrarion-level syntax tests. If the docs are
# not sufficiently clear, please tell me so that I can fix them. See the main
# documentation for contact channels.

use strict;
use warnings;
use utf8;

use Test::More 1;
use Test::Exception;
use Util::Underscore v1.3.0;

use constant THE_CLASS => 'MarpaX::Grammar::Preprocessor::Parser';

use_ok THE_CLASS;

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

sub _fixture_handle_source_ref {
    my ($args, $fixture) = @_;

    if (exists $args->{source_ref}) {
        my $source_ref = delete $args->{source_ref};
        my $postcondition = undef;
        return ($source_ref, $postcondition);
    }

    my $input_before = delete $args->{input_before} // '****';
    my $input_middle = delete $args->{input} // die "argument input required";
    my $input_after  = delete $args->{input_after} // '****';
    my $input = $input_before . $input_middle . $input_after;

    my $pos = length $input_before;
    my $pos_expected = $pos + (delete $args->{pos_expected} // length $input_middle);

    my $pos_message = delete $args->{pos_message} // do {
        'match position ' . ((length $input_middle) ? 'advanced' : 'unchanged');
    };

    pos($input) = $pos;
    my $source_ref = \$input;

    {
        $fixture->{pos} = $pos;
        my $context_seen = $input_before . $input_middle;
        $context_seen =~ /(.{0,20})\z/s and $fixture->{context_seen} = $1;
        $input_after  =~ /\A(.{0,20})/s and $fixture->{context_next} = $1;
        my $line_contents;
        $context_seen =~ /([^\n]*)\z/ and $line_contents .= $1;
        $input_after  =~ /\A([^\n]*)/ and $line_contents .= $1;
        $fixture->{line_contents} = $line_contents;
    }

    my $postcondition = sub {
        is pos($$source_ref), $pos_expected, $pos_message;
    };

    return ($source_ref, $postcondition);
}

sub _fixture_handle_buffers {
    my ($args, $fixture) = @_;

    my $buffers = delete $args->{buffers} // die "argument buffers required";
    if (@$buffers != 2) {
        die "argument buffers requires an arrayref of exactly two buffers";
    }
    my $buffers_unchanged = 0;
    my $buffers_expected = delete $args->{buffers_expected} // do {
        $buffers_unchanged = 1;
        [@$buffers]
    };
    my $buffers_message = delete $args->{buffers_message} // do {
        die "argument buffers_message required" if not $buffers_unchanged;
        'buffers are unchanged';
    };
    my $postcondition = sub {
        my ($parser) = @_;
        my $buffers = [
            ${ $parser->_MarpaX_Grammar_Preprocessor_Parser_buffer },
            ${ $parser->_MarpaX_Grammar_Preprocessor_Parser_buffer_deferred },
        ];
        is_deeply $buffers, $buffers_expected, $buffers_message;
    };

    return ($buffers, $postcondition);
}

sub _fixture_handle_method {
    my ($args, $fixture) = @_;

    my $method = delete $args->{method} // return;
    my $method_args = delete $args->{method_args} // [];
    my $message = delete $args->{message} // 'got SLIF snippet';

    my $main_test;
    if (my $throws_spec = delete $args->{throws_ok}) {
        if (_::is_regex $throws_spec) {
            my $regex = $throws_spec;
            $message //= $regex;
            $main_test = sub {
                my ($parser) = @_;
                throws_ok { $parser->$method(@$method_args) } $regex, $message;
            };
        }
        elsif (_::is_hash_ref $throws_spec) {
            my %throws_args = %$throws_spec;
            my $class = delete $throws_args{class};
            $message //= sprintf "throws %s exception object", $class // "unknown";

            if (defined $class and $class eq 'MarpaX::Grammar::Preprocessor::ParseException') {
                $throws_args{context_seen} = $fixture->{context_seen}
                    if not exists $throws_args{context_seen};
                $throws_args{context_next} = $fixture->{context_next}
                    if not exists $throws_args{context_next};
                $throws_args{line_contents} = $fixture->{line_contents}
                    if not exists $throws_args{line_contents};
            }

            $main_test = sub {
                my ($parser) = @_;
                dies_ok { $parser->$method(@$method_args) } $message
                    or return;
                my $err = $@;

                isa_ok $err, $class if defined $class;
                for my $field (sort keys %throws_args) {
                    my $expected = $throws_args{$field};
                    is $err->$field, $expected, "error $field matches";
                }
            };
        }
        else {
            die "named argument throws_ok must be regex or hash ref";
        }
    }
    elsif (my $expected = delete $args->{expected})
    {
        $message //= 'got SLIF snippet';
        $main_test = sub {
            my ($parser) = @_;
            my @result = $parser->$method(@$method_args);
            is_deeply \@result, $expected, $message;
        };
    }
    else {
        die "expected named argument expected or throws_ok";
    }

    exists $args->{test} and die "named argument test cannot be used in parser_fixture";

    $args->{test} = sub {
        my ($parser) = @_;

        my $source_ref = $parser->_MarpaX_Grammar_Preprocessor_Parser_source_ref;
        for ($source_ref ? $$source_ref : undef) {
            $main_test->($parser);
        }
    };

    return;
}

sub parser_fixture {
    my (%args) = @_;
    my %fixture;

    my ($source_ref, $source_ref_postcondition) =
        _fixture_handle_source_ref(\%args, \%fixture);

    _fixture_handle_method(\%args, \%fixture);

    my $test = delete $args{test} // die "argument test required";

    my ($buffers, $buffers_postcondition) =
        _fixture_handle_buffers(\%args, \%fixture);

    my $class = delete $args{class} // THE_CLASS;

    my $ctor_args = delete $args{ctor_args} // {};

    my $postcondition = delete $args{postcondition};

    if (my @keys = sort keys %args) {
        die "Unknown named arguments: @keys, died";
    }

    return sub {
        my ($buffer_main, $buffer_deferred) = @$buffers;
        my $parser = $class->new(
            source_ref => $source_ref,
            buffer => \$buffer_main,
            buffer_deferred => \$buffer_deferred,
            %$ctor_args,
        );

        $test->($parser, \%fixture);

        $_->($parser) for grep { $_ }
            $buffers_postcondition,
            $source_ref_postcondition,
            $postcondition;

        return;
    };
}

describe result => sub {
    it 'returns a Result instance' => parser_fixture
        source_ref => undef,
        buffers => ['foo', ''],
        test => sub {
            my ($parser) = @_;

            my $result = $parser->result;

            is _::class $result, 'MarpaX::Grammar::Preprocessor::Result', 'result is an object';
        };

    it 'contains the buffer contents' => parser_fixture
        source_ref => undef,
        buffers => ['the contents', ''],
        test => sub {
            my ($parser) = @_;

            my $result = $parser->result;

            is $result->slif_source, 'the contents', 'got the buffer contents';
        };

    my $expected_docs = {};
    it 'returns the documentation hash' => parser_fixture
        source_ref => undef,
        buffers => ['foo', ''],
        ctor_args => {
            _MarpaX_Grammar_Preprocessor_Parser_docs => $expected_docs,
        },
        test => sub {
            my ($parser) = @_;

            my $result = $parser->result;

            is $result->docs, $expected_docs, 'got the documentation';
        };
};

describe new_with => sub {
    it 'creates a shallow copy of the object' => sub {
        my $source = 'foo';
        my $orig = THE_CLASS->new(
            source_ref => \$source,
            buffer => undef,
            buffer_deferred => undef,
        );

        my $copy = $orig->new_with();

        is  $copy->_MarpaX_Grammar_Preprocessor_Parser_source_ref,
            $orig->_MarpaX_Grammar_Preprocessor_Parser_source_ref,
            'copy has same source reference';
        is  $copy->_MarpaX_Grammar_Preprocessor_Parser_namespace,
            $orig->_MarpaX_Grammar_Preprocessor_Parser_namespace,
            'copy has same namespace';
        is  $copy->_MarpaX_Grammar_Preprocessor_Parser_namespace_separator,
            $orig->_MarpaX_Grammar_Preprocessor_Parser_namespace_separator,
            'copy has same namespace_separator';
        is  $copy->_MarpaX_Grammar_Preprocessor_Parser_docs,
            $orig->_MarpaX_Grammar_Preprocessor_Parser_docs,
            'copy has same docs';
        is  $copy->_MarpaX_Grammar_Preprocessor_Parser_optional_rule_cache,
            $orig->_MarpaX_Grammar_Preprocessor_Parser_optional_rule_cache,
            'copy has same optional_rule_cache';
    };

    it 'can override arguments' => sub {
        _::const my $source => 'foo';
        _::const my $orig => THE_CLASS->new(
            source_ref => \$source,
            buffer => undef,
            buffer_deferred => undef,
        );
        _::const my $new_namespace => 'new namespace';
        _::const my $new_namespace_separator => '::';

        my $copy = $orig->new_with(
            namespace => $new_namespace,
            namespace_separator => $new_namespace_separator,
        );

        is  $copy->_MarpaX_Grammar_Preprocessor_Parser_source_ref,
            $orig->_MarpaX_Grammar_Preprocessor_Parser_source_ref,
            'copy has same source reference';
        is  $copy->_MarpaX_Grammar_Preprocessor_Parser_namespace,
            $new_namespace,
            'copy has new namespace';
        is  $copy->_MarpaX_Grammar_Preprocessor_Parser_namespace_separator,
            $new_namespace_separator,
            'copy has new namespace_separator';
        is  $copy->_MarpaX_Grammar_Preprocessor_Parser_docs,
            $orig->_MarpaX_Grammar_Preprocessor_Parser_docs,
            'copy has same docs';
        is  $copy->_MarpaX_Grammar_Preprocessor_Parser_optional_rule_cache,
            $orig->_MarpaX_Grammar_Preprocessor_Parser_optional_rule_cache,
            'copy has same optional_rule_cache';
    };

    it 'works correctly with subclasses' => sub {
        BEGIN {
            package Local::ParserSubclassForNewWith;
            use Moo;
            extends 'MarpaX::Grammar::Preprocessor::Parser';
            use namespace::clean;

            has _custom_state => (is => 'ro', required => 1);

            around new_with => sub {
                my ($orig, $self, %args) = @_;
                return $self->$orig(
                    _custom_state => $self->_custom_state,
                    %args,
                );
            };
        }

        my $orig = Local::ParserSubclassForNewWith->new(
            _custom_state => 42,
            source_ref => undef,
            buffer => undef,
            buffer_deferred => undef,
        );

        my $copy = $orig->new_with(source_ref => \("foo"));

        ok +(_::is_instance $copy, 'Local::ParserSubclassForNewWith'),
            'got correct class'
            or diag sprintf "got %s instance but expected %s",
                (_::class $copy) // "<undef>",
                'Local::ParserSubclassForNewWith';
        is_deeply $copy->_MarpaX_Grammar_Preprocessor_Parser_source_ref,
            \('foo'),
            'got overridden source_ref value';
        is  $copy->_MarpaX_Grammar_Preprocessor_Parser_namespace,
            $orig->_MarpaX_Grammar_Preprocessor_Parser_namespace,
            'copy has same namespace';
        is  $copy->_MarpaX_Grammar_Preprocessor_Parser_namespace_separator,
            $orig->_MarpaX_Grammar_Preprocessor_Parser_namespace_separator,
            'copy has same namespace_separator';
        is  $copy->_MarpaX_Grammar_Preprocessor_Parser_docs,
            $orig->_MarpaX_Grammar_Preprocessor_Parser_docs,
            'copy has same docs';
        is  $copy->_MarpaX_Grammar_Preprocessor_Parser_optional_rule_cache,
            $orig->_MarpaX_Grammar_Preprocessor_Parser_optional_rule_cache,
            'copy has same optional_rule_cache';
        is  $copy->_custom_state,
            $orig->_custom_state,
            'copy has same _custom_state';
    };
};

describe write => sub {
    it 'appends data to the primary buffer' => parser_fixture
        source_ref => undef,
        buffers => ['a', 'b'],
        buffers_expected => ['a12', 'b'],
        buffers_message => 'the strings where appended to the main buffer',
        test => sub {
            my ($parser) = @_;

            $parser->write('1', '2');
        };
};

describe write_deferred => sub {
    it 'appends data to the deferred buffer' => parser_fixture
        source_ref => undef,
        buffers => ['a', 'b'],
        buffers_expected => ['a', 'b12'],
        buffers_message => 'the strings where appended to the main buffer',
        test => sub {
            my ($parser) = @_;

            $parser->write_deferred('1', '2');
        };
};

describe next_token => sub {
    it 'returns EOF and the end of input' => sub {
        case 'empty string' => parser_fixture
            input_before => '',
            input => '',
            input_after => '',
            buffers => [undef, undef],
            test => sub {
                my ($parser) = @_;

                my @result = $parser->next_token;

                is_deeply \@result, [EOF => undef], 'returned EOF token';
            };

        case 'end of string' => parser_fixture
            input => '',
            input_after => '',
            buffers => [undef, undef],
            test => sub {
                my ($parser) = @_;

                my @result = $parser->next_token;

                is_deeply \@result, [EOF => undef], 'returned EOF token';
            };
    };

    it 'throws when no explicit pos() was defined' => parser_fixture
        method => 'next_token',
        source_ref => \"",
        buffers => [undef, undef],
        throws_ok => {
            class => 'MarpaX::Grammar::Preprocessor::Exception',
            message => 'input source match position was not defined',
        };

    it 'appends whitespace and comments to buffer, then returns next token' => sub {
        my $case = sub {
            my ($whitespace) = @_;

            case 'before EOF' => parser_fixture
                input => $whitespace,
                input_after => '',
                buffers => ['a', 'b'],
                buffers_expected => ['a'.$whitespace, 'b'],
                buffers_message => 'space was appended to buffer',
                test => sub {
                    my ($parser) = @_;

                    my @result = $parser->next_token;

                    is_deeply \@result, [EOF => undef], 'next token was EOF';
                };

            case 'before IDENT' => parser_fixture
                input => $whitespace . 'name',
                buffers => ['a', 'b'],
                buffers_expected => ['a'.$whitespace, 'b'],
                buffers_message => 'space was appended to buffer',
                test => sub {
                    my ($parser) = @_;

                    my @result = $parser->next_token;

                    is_deeply \@result, [IDENT => 'name'], 'next token was IDENT';
                };
        };

        case 'whitespace' => sub { $case->('    ') };
        case 'comments' => sub { $case->("# helpful description\n") };
    };

    it 'returns simple identifiers' => parser_fixture
        input => 'foo_bar',
        buffers => ['a', 'b'],
        test => sub {
            my ($parser) = @_;

            my @result = $parser->next_token;

            is_deeply \@result, [IDENT => 'foo_bar'], 'got IDENT';
        };

    it 'returns namespaced identifiers' => parser_fixture
        input => '%Foo',
        buffers => ['a', 'b'],
        ctor_args => {
            namespace => 'TheNamespace',
        },
        test => sub {
            my ($parser) = @_;
            is $parser->_MarpaX_Grammar_Preprocessor_Parser_namespace,
                'TheNamespace',
                'precondition: have namespace';
            my $sep = $parser->_MarpaX_Grammar_Preprocessor_Parser_namespace_separator;

            my @result = $parser->next_token;

            is_deeply \@result, [IDENT => "TheNamespace${sep}Foo"], 'got namespaced ident';
        };

    it 'returns namespace references' => parser_fixture
        input => '%',
        buffers => ['a', 'b'],
        ctor_args => {
            namespace => 'TheNamespace',
        },
        test => sub {
            my ($parser) = @_;
            is $parser->_MarpaX_Grammar_Preprocessor_Parser_namespace,
                'TheNamespace',
                'precondition: has namespace';

            my @result = $parser->next_token;

            is_deeply \@result, [IDENT => 'TheNamespace'], 'got the namespace';
        };

    it 'supports relative namespaced identifiers' => sub {
        my $test = sub {
            my ($namespace, $input, $expected) = @_;
            return parser_fixture
                method => 'next_token',
                input => $input,
                buffers => ['a', 'b'],
                ctor_args => {
                    namespace => $namespace,
                },
                expected => [IDENT => $expected];
        };

        case 'one up' => $test->(Foo__Bar => '%..foo', 'Foo__foo');
        case 'one up to root' => $test->(Foo => '%..foo', 'foo');
        case 'three up' => $test->(Foo__Bar__Baz__Qux => '%....foo', 'Foo__foo');
    };

    it 'dies if no namespace was set' => parser_fixture
        method => 'next_token',
        input => '%',
        buffers => [undef, undef],
        throws_ok => {
            class => 'MarpaX::Grammar::Preprocessor::ParseException',
            message => 'Namespaced identifiers can only be used inside a namespace',
        };

    it 'returns literal strings' => parser_fixture
        input => q*'foo\\\\bar\\'*, # actually 'foo\\bar\'
        buffers => ['a', 'b'],
        test => sub {
            my ($parser) = @_;

            my @result = $parser->next_token;

            is_deeply \@result, [LITERAL => q('foo\\\\bar\\')], 'got correct string';
        };

    it 'returns literal strings with case insensitive modifier' => parser_fixture
        input => q*'foo\\\\bar\\':i*, # actually 'foo\\bar\':i
        buffers => ['a', 'b'],
        test => sub {
            my ($parser) = @_;

            my @result = $parser->next_token;

            is_deeply \@result, [LITERAL => q('foo\\\\bar\\':i)], 'got correct string';
        };

    it 'returns character classes' => parser_fixture
        input => '[^\\w\\\\a-z\\-\\]\\[{]', # actually [^\w\\a-z\-\]\[{]
        buffers => ['a', 'b'],
        test => sub {
            my ($parser) = @_;

            my @result = $parser->next_token;

            is_deeply \@result, [LITERAL => q([^\\w\\\\a-z\\-\\]\\[{])], 'got correct charclass';
        };

    it 'dispatches docstrings to doc command' => parser_fixture
        input => '',
        input_after => '"""****',
        buffers => ['a', 'b'],
        test => sub {
            my ($parser, $fixture) = @_;
            my $actual_position;

            # override the command_doc method in the current dynamic scope.
            local *MarpaX::Grammar::Preprocessor::Parser::command_doc;
            *MarpaX::Grammar::Preprocessor::Parser::command_doc = sub {
                my ($self) = @_;
                $actual_position = pos();
                return IDENT => 'Foo';
            };

            my @result = $parser->next_token;

            is_deeply \@result, [IDENT => 'Foo'], 'got the expected return value';
            is $actual_position, $fixture->{pos}, 'doc command got correct position';
        };

    it 'delegates to commands' => parser_fixture
        input => '\\test foo', # actually \test foo
        buffers => ['a', 'b'],
        test => sub {
            my ($parser, $fixture) = @_;

            my $actually_called;
            my $actual_position;

            local *MarpaX::Grammar::Preprocessor::Parser::command_test;
            *MarpaX::Grammar::Preprocessor::Parser::command_test = sub {
                my ($self) = @_;
                $actually_called = 1;
                $actual_position = pos();
                /\G \s* (foo)\b/xgc
                    or die "Expected foo";
                return OP => "${1}_bar";
            };

            my @result = $parser->next_token;

            ok $actually_called, 'proper command was invoked';
            is_deeply \@result, [OP => 'foo_bar'], 'got parse result';
            is $actual_position, $fixture->{pos} + length '\\test', 'command got correct position';
        };

    it 'throws for unknown commands' => parser_fixture
        method => 'next_token',
        input => '\\this_command_does_not_exist_I_hope',
        buffers => ['a', 'b'],
        throws_ok => {
            class => 'MarpaX::Grammar::Preprocessor::ParseException',
            message => 'Unknown command \\this_command_does_not_exist_I_hope, could not find command_this_command_does_not_exist_I_hope() method'
        };

    it 'recurses into inline rules' => parser_fixture
        input => q({ the_name bleh, parser don't care' }),
        buffers => ['a', 'b'],
        buffers_expected => ['a', q(b the_name bleh, parser don't care' ; )],
        buffers_message => 'rule body was appended to deferred buffer',
        test => sub {
            my ($parser) = @_;

            my @result = $parser->next_token;

            is_deeply \@result, [IDENT => 'the_name'], 'inline rule returns its identifier';
        };

    it 'throws when inline rules aren\'t properly terminated' => parser_fixture
        method => 'next_token',
        input => q({ the_name bleh),
        input_after => '',
        buffers => ['a', 'b'],
        throws_ok => {
            class => 'MarpaX::Grammar::Preprocessor::ParseException',
            message => 'Expecting closing brace for inline rule, but got EOF',
        };

    it 'returns close tokens' => parser_fixture
        input => '}',
        buffers => ['a', 'b'],
        test => sub {
            my ($parser) = @_;

            my @result = $parser->next_token;

            is_deeply \@result, [CLOSE => undef], 'got CLOSE token';
        };

    it 'returns anything else as operators' => parser_fixture
        input => '::=|+*=>',
        input_after => 'glorp',
        buffers => ['a', 'b'],
        test => sub {
            my ($parser) = @_;

            my @result = $parser->next_token;

            is_deeply \@result, [OP => '::=|+*=>'], 'got OP token';
        };

    it 'tries to explain impossible syntax errors' => parser_fixture
        method => 'next_token',
        input => '',
        input_after => q(''345678901234567890),
        buffers => ['a', 'b'],
        throws_ok => {
            class => 'MarpaX::Grammar::Preprocessor::ParseException',
            message => 'Illegal code path taken: unknown grammar string',
        };
};

BEGIN {
    package Local::Parser::MockNextToken;
    use Moo;
    extends 'MarpaX::Grammar::Preprocessor::Parser';

    my $coerce = sub { MarpaX::Grammar::Preprocessor::TokenType->coerce(@_) };

    has mock_tokens => (
        is => 'ro',
        required => 1,
    );

    around new_with => sub {
        my ($orig, $self, %args) = @_;
        return $self->$orig(
            mock_tokens => $self->mock_tokens,
            %args,
        );
    };

    sub next_token {
        my ($self) = @_;
        my $mock_tokens = $self->mock_tokens;
        return $coerce->('EOF'), undef if not @$mock_tokens;
        return $coerce->('EOF'), undef if not $mock_tokens->[0];
        my $pair = shift @$mock_tokens;
        my ($type, $value) = (ref $pair eq ref sub{}) ? $pair->() : @$pair;
        return $coerce->($type), $value;
    }
}

describe pump => sub {
    it 'consumes the whole input' => parser_fixture
        method => 'pump',
        method_args => ['EOF'],
        source_ref => undef,
        class => 'Local::Parser::MockNextToken',
        ctor_args => {
            mock_tokens => [
                [OP => 'a'],
                [LITERAL => 'b'],
                [IDENT => 'c'],
                undef, # to terminate the pump
                sub { die "should never be called" },
            ],
        },
        expected => [EOF => undef],
        message => 'returned empty list',
        buffers => ['prelude:', undef],
        buffers_expected => ['prelude:abc', undef],
        buffers_message => 'input was consumed';

    it 'can terminate on CLOSE tokens' => parser_fixture
        method => 'pump',
        method_args => ['CLOSE'],
        source_ref => undef,
        class => 'Local::Parser::MockNextToken',
        ctor_args => {
            mock_tokens => [
                [OP => 'a'],
                [LITERAL => 'b'],
                [IDENT => 'c'],
                [CLOSE => undef],
                sub { die "should never be called" },
            ],
        },
        expected => [CLOSE => undef],
        message => 'returned CLOSE token',
        buffers => ['prelude:', undef],
        buffers_expected => ['prelude:abc', undef],
        buffers_message => 'input was consumed';
};

describe expect => sub {
    it 'returns the token if it has the expected type' => parser_fixture
        source_ref => undef,
        class => 'Local::Parser::MockNextToken',
        ctor_args => { mock_tokens => [[IDENT => 'the value']] },
        buffers => [undef, undef],
        test => sub {
            my ($parser) = @_;

            my @result = $parser->expect('IDENT');

            is_deeply \@result, ['the value'], 'got expected value';
        };

    it 'accepts multiple possible types' => parser_fixture
        source_ref => undef,
        class => 'Local::Parser::MockNextToken',
        ctor_args => { mock_tokens => [[IDENT => 'the value']] },
        buffers => [undef, undef],
        test => sub {
            my ($parser) = @_;

            my @result = $parser->expect(qw/LITERAL IDENT OP/);

            is_deeply \@result, ['the value'], 'got expected value';
        };

    it 'dies when the token type is not accepted' => parser_fixture
        method => 'expect',
        method_args => [qw/LITERAL OP/],
        source_ref => undef,
        class => 'Local::Parser::MockNextToken',
        ctor_args => { mock_tokens => [[IDENT => 'the value']] },
        buffers => [undef, undef],
        throws_ok => {
            class => 'MarpaX::Grammar::Preprocessor::ParseException',
            message => 'expected {LITERAL, OP} but found IDENT',
        };

    it 'has error messages pointing to the beginning of the token' => parser_fixture
        method => 'expect',
        method_args => ['LITERAL', 'OP'],
        input => '',
        input_after => 'foo****', # an IDENT
        buffers => [undef, undef],
        throws_ok => {
            class => 'MarpaX::Grammar::Preprocessor::ParseException',
            message => 'expected {LITERAL, OP} but found IDENT',
        };

    it 'dies if no tokens are available' => parser_fixture
        method => 'expect',
        method_args => [qw/ LITERAL OP /],
        source_ref => undef,
        class => 'Local::Parser::MockNextToken',
        ctor_args => { mock_tokens => [] },
        buffers => [undef, undef],
        throws_ok => {
            class => 'MarpaX::Grammar::Preprocessor::ParseException',
            message => 'expected {LITERAL, OP} but found EOF',
        };
};

describe command_array => parser_fixture
    method => 'command_array',
    source_ref => undef,
    expected => [OP => 'action => ::array'],
    buffers => ['a', 'b'];

describe command_null => parser_fixture
    method => 'command_null',
    source_ref => undef,
    expected => [OP => 'action => ::undef'],
    buffers => ['a', 'b'];

describe command_group => parser_fixture
    method => 'command_group',
    source_ref => undef,
    expected => [OP => 'assoc => group'],
    buffers => ['a', 'b'];

describe command_left => parser_fixture
    method => 'command_left',
    source_ref => undef,
    expected => [OP => 'assoc => left'],
    buffers => ['a', 'b'];

describe command_right => parser_fixture
    method => 'command_right',
    source_ref => undef,
    expected => [OP => 'assoc => right'],
    buffers => ['a', 'b'];

describe command_lax => parser_fixture
    method => 'command_lax',
    source_ref => undef,
    expected => [OP => 'proper => 0'],
    buffers => ['a', 'b'];

describe command_do => sub {
    it 'parses the action name' => parser_fixture
        method => 'command_do',
        input => ' FooBar',
        expected => [OP => 'action => do_FooBar'],
        buffers => ['a', 'b'];

    it 'dies when no action name could be found' => parser_fixture
        method => 'command_do',
        input => '',
        input_after => ' %glorp',
        buffers => ['a', 'b'],
        throws_ok => {
            class => 'MarpaX::Grammar::Preprocessor::ParseException',
            message => 'Expected action name',
        };
};

describe command_sep => parser_fixture
    method => 'command_sep',
    source_ref => undef,
    class => 'Local::Parser::MockNextToken',
    ctor_args => {
        mock_tokens => [
            [IDENT => 'some_identifier'],
            sub { die "should never be seen" },
        ],
    },
    expected => [OP => 'separator => some_identifier'],
    buffers => ['a', 'b'];

describe command_keyword => parser_fixture
    method => 'command_keyword',
    source_ref => undef,
    class => 'Local::Parser::MockNextToken',
    ctor_args => {
        mock_tokens => [
            [IDENT => 'some_identifier'],
            sub { die "should never be seen" },
        ],
    },
    expected => [IDENT => 'some_identifier'],
    message => 'passes through identifier',
    buffers => ['a', 'b'],
    buffers_expected => ['a:lexeme ~ some_identifier priority => 1;', 'b'],
    buffers_message => 'appends keyword prioritization to immediate buffer';

describe command_namespace => sub {
    it 'expands to the body' => parser_fixture
        method => 'command_namespace',
        input => '{',
        class => 'Local::Parser::MockNextToken',
        ctor_args => {
            mock_tokens => [
                [IDENT => 'some_identifier'],
                # [OPEN => '{'], matched from the input source_ref
                [LITERAL => 'foo'],
                [CLOSE => undef],
                sub { die "should never be seen" },
            ],
        },
        expected => [OP => 'foo'],
        message => 'expands to body',
        buffers => ['a', 'b'];

    it 'uses the new namespace within the body' => parser_fixture
        method => 'command_namespace',
        input => 'Foo { % }',
        ctor_args => {
            namespace => 'Qux',
        },
        expected => [OP => ' Foo '],
        message => 'namespace is set within the body',
        buffers => ['a', 'b'];

    it 'supports nested namespaces' => parser_fixture
        method => 'command_namespace',
        input => '%Foo { % %bar }',
        ctor_args => {
            namespace => 'Qux',
        },
        expected => [OP => ' Qux__Foo Qux__Foo__bar '],
        message => 'namespaces can be nested',
        buffers => ['a', 'b'];
};

describe command_doc => sub {
    it 'can parse multiline docstrings' => parser_fixture
        method => 'command_doc',
        input =>
            qq( """ foo\n) .
            qq("""   bar\n) .
            qq(      """ baz\n),
        input_after => qq( "" qux),
        class => 'Local::Parser::MockNextToken',
        ctor_args => {
            mock_tokens => [
                [IDENT => 'some_identifier'],
                sub { die "should never be seen" },
            ],
            _MarpaX_Grammar_Preprocessor_Parser_docs=> { foo => "foo docs" },
        },
        expected => [IDENT => 'some_identifier'],
        message => 'passes through identifier',
        buffers => ['a', 'b'],
        postcondition => sub {
            my ($parser) = @_;
            my $docs = $parser->_MarpaX_Grammar_Preprocessor_Parser_docs,
            my $expected_docs = {
                foo => "foo docs",
                some_identifier => "foo\n  bar\nbaz",
            };
            is_deeply $docs, $expected_docs, 'stored documentation matches';
        };

    it 'can hide symbols' => parser_fixture
        method => 'command_doc',
        input => 'hide',
        class => 'Local::Parser::MockNextToken',
        ctor_args => {
            mock_tokens => [
                [IDENT => 'some_identifier'],
                sub { die "should never be seen" },
            ],
            _MarpaX_Grammar_Preprocessor_Parser_docs=> { foo => "foo docs" },
        },
        expected => [IDENT => 'some_identifier'],
        message => 'passes through identifier',
        buffers => ['a', 'b'],
        postcondition => sub {
            my ($parser) = @_;
            my $docs = $parser->_MarpaX_Grammar_Preprocessor_Parser_docs,
            my $expected_docs = {
                foo => "foo docs",
                some_identifier => undef,
            };
            is_deeply $docs, $expected_docs, 'stored documentation matches';
        };

    it 'throws on illegal syntax' => parser_fixture
        method => 'command_doc',
        input => '',
        input_after => 'florp6789012345678901',
        class => 'Local::Parser::MockNextToken',
        ctor_args => {
            mock_tokens => [
                sub { die "should never be seen" },
            ],
            _MarpaX_Grammar_Preprocessor_Parser_docs=> { foo => "foo docs" },
        },
        buffers => ['a', 'b'],
        throws_ok => {
            class => 'MarpaX::Grammar::Preprocessor::ParseException',
            message => 'Expected docstring',
        },
        postcondition => sub {
            my ($parser) = @_;
            my $docs = $parser->_MarpaX_Grammar_Preprocessor_Parser_docs,
            my $expected_docs = {
                foo => "foo docs",
            };
            is_deeply $docs, $expected_docs, 'stored documentation matches';
        };

    it 'throws when symbol was already defined' => parser_fixture
        method => 'command_doc',
        input => 'hide',
        class => 'Local::Parser::MockNextToken',
        ctor_args => {
            mock_tokens => [
                [IDENT => 'some_identifier'],
                sub { die "should never be seen" },
            ],
            _MarpaX_Grammar_Preprocessor_Parser_docs=> { some_identifier => "existing docs" },
        },
        throws_ok => qr/\A\QSymbol some_identifier already documented\E\b/,
        buffers => ['a', 'b'],
        postcondition => sub {
            my ($parser) = @_;
            my $docs = $parser->_MarpaX_Grammar_Preprocessor_Parser_docs,
            my $expected_docs = {
                some_identifier => 'existing docs',
            };
            is_deeply $docs, $expected_docs, 'stored documentation matches';
        };
};

describe command_optional => sub {
    it 'generates optional rules' => parser_fixture
        method => 'command_optional',
        source_ref => undef,
        class => 'Local::Parser::MockNextToken',
        ctor_args => {
            mock_tokens => [
                [IDENT => 'some_identifier'],
                sub { die "should never be seen" },
            ],
        },
        expected => [IDENT => 'some_identifier__Optional'],
        message => 'returned name of optional symbol',
        buffers => ['a;', 'b;'],
        buffers_expected => ['a;', 'b;some_identifier__Optional ::= action => ::undef; some_identifier__Optional ::= some_identifier action => ::first; '],
        buffers_message => 'optional rule was written to deferred buffer';

    it 'caches optional rules' => parser_fixture
        method => 'command_optional',
        source_ref => undef,
        class => 'Local::Parser::MockNextToken',
        ctor_args => {
            mock_tokens => [
                [IDENT => 'some_identifier'],
                sub { die "should never be seen" },
            ],
            _MarpaX_Grammar_Preprocessor_Parser_optional_rule_cache => {
                some_identifier => 'optional_rule_name',
            },
        },
        expected => [IDENT => 'optional_rule_name'],
        message => 'returned optional symbol from cache',
        buffers => ['a', 'b'];
};

describe command_include => sub {
    it 'works' => parser_fixture
        method => 'command_include',
        input => 'TheNamespace = test-include.bnf',
        input_after => ' ***',
        ctor_args => {
            file_loader => sub {
                my ($name) = @_;
                is $name, 'test-include.bnf', 'got expected file name';
                return '%symbol ~ is_awesome';
            },
        },
        buffers => ['a', 'b'],
        expected => [OP => 'TheNamespace__symbol ~ is_awesome'],
        message => 'file contents are processed as a namespace';

    it 'throws when no file loader was specified' => parser_fixture
        method => 'command_include',
        input => 'Foo = bar',
        input_after => ' ***',
        buffers => ['a', 'b'],
        throws_ok => {
            class => 'MarpaX::Grammar::Preprocessor::Exception',
            message => 'The \include command requires that a file_loader was specified',
        },
        message => "include throws without file loader";

    it 'throws when the equals sign was forgotten' => parser_fixture
        method => 'command_include',
        input => 'Foo',
        input_after => ' foo',
        buffers => ['a', 'b'],
        throws_ok => {
            class => 'MarpaX::Grammar::Preprocessor::ParseException',
            message => 'Expected equals sign "=" after namespace in \include command',
        };

    it 'throws when no file name was found' => parser_fixture
        method => 'command_include',
        input => 'Foo =',
        input_after => '  ',
        buffers => ['a', 'b'],
        throws_ok => {
            class => 'MarpaX::Grammar::Preprocessor::ParseException',
            message => 'Expected file name in \include command',
        };
};

done_testing;
