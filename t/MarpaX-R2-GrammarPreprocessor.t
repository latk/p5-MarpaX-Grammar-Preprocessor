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

use constant THE_CLASS => 'MarpaX::R2::GrammarPreprocessor';

use_ok THE_CLASS;

sub describe {
    my ($name, $body) = @_;
    return subtest "describe $name", $body;
}

sub it {
    my ($name, $body) = @_;
    return subtest "it $name", $body;
}

sub case {
    my ($name, $body) = @_;
    return subtest "case $name", $body;
}

_::const my $__namespace__  => THE_CLASS->_test_accessors('namespace');
_::const my $__docs__       => THE_CLASS->_test_accessors('docs');
_::const my $__source_ref__ => THE_CLASS->_test_accessors('source_ref');
_::const my $__buffers__    => THE_CLASS->_test_accessors('buffers');

describe constants => sub {
    for my $NAME (qw/IDENT LITERAL OP CLOSE/) {
        describe $NAME => sub {
            ok defined THE_CLASS->$NAME, 'has constant';
            is THE_CLASS->TOKEN_TYPES->{$NAME}, THE_CLASS->$NAME, 'can look constant up by name';
            is THE_CLASS->TOKEN_NAMES->{THE_CLASS->$NAME}, $NAME, 'can look up name by id';
        };
    }
};

describe _get_fresh_instance => sub {
    it 'creates an instance' => sub {
        my $self = THE_CLASS->_get_fresh_instance;

        ok _::blessed $self, 'instance is an object';
        isa_ok $self, THE_CLASS, 'instance is of expected class';
    };

    it 'initializes namespace' => sub {
        my $self = THE_CLASS->_get_fresh_instance;

        my $namespace = $self->$__namespace__;
        is $namespace, undef, 'the namespace is undef';
    };

    it 'initializes the source ref' => sub {
        my $dummy;

        my $self = THE_CLASS->_get_fresh_instance(\$dummy);

        my $source_ref = $self->$__source_ref__;
        is $source_ref, \$dummy, 'sets the source ref';
    };

    it 'initializes the docs' => sub {
        my $self = THE_CLASS->_get_fresh_instance;

        my $docs = $self->$__docs__;
        is_deeply $docs, {}, 'initialized docs to empty hash';
    };

    it 'loads a prelude into the buffer' => sub {
        my $self = THE_CLASS->_get_fresh_instance;

        my $buffers = $self->$__buffers__;
        my $expected = $self->SLIF_PRELUDE;
        is_deeply $buffers, [$expected], 'buffers contain the prelude';
    };
};

package Local::MockNextToken {
    use Moo;
    extends 'MarpaX::R2::GrammarPreprocessor';

    has mock_tokens => (
        is => 'ro',
        required => 1);

    sub next_token {
        my ($self) = @_;
        return @{ shift @{ $self->mock_tokens } };
    }
}

describe _pump => sub {
    my @tokens = (
        [undef, 'a'],
        [undef, 'b'],
        [undef, 'c'],
        [], # to terminate the pump
    );
    my $self = Local::MockNextToken->new(mock_tokens => \@tokens);

    $self->_pump;

    my $expected = $self->SLIF_PRELUDE . 'abc' . '; ';
    is $self->slif_source, $expected, 'the tokens were passed through';
    is_deeply \@tokens, [], 'all tokens where consumed';
};

package Local::PreprocessMock {
    use Moo;
    extends 'MarpaX::R2::GrammarPreprocessor';

    use constant EXPECTED_SOURCE => 'the source';
    our $WAS_CALLED = 0;

    sub _pump {
        my ($self) = @_;
        $WAS_CALLED = 1;
        my $source_ref = $self->$__source_ref__;
        Test::More::is $$source_ref, EXPECTED_SOURCE, 'got the expected input source';
    }
}

describe preprocess => sub {
    it 'can take strings' => sub {
        local $Local::PreprocessMock::WAS_CALLED = 0;
        my $self = Local::PreprocessMock->preprocess(Local::PreprocessMock::EXPECTED_SOURCE);
        is _::blessed $self, 'Local::PreprocessMock', 'got the object back';
        ok $Local::PreprocessMock::WAS_CALLED, '_pump was called',
    };

    it 'can take file handles' => sub {
        local $Local::PreprocessMock::WAS_CALLED = 0;
        my $source_copy = Local::PreprocessMock::EXPECTED_SOURCE;
        open my $fh, "<", \$source_copy
            or die "Could not open PerlIO file handle: $!";
        my $self = Local::PreprocessMock->preprocess($fh);
        is _::blessed $self, 'Local::PreprocessMock', 'got the object back';
        ok $Local::PreprocessMock::WAS_CALLED, '_pump was called',
    };
};

describe buffer_push => sub {
    it 'initializes the next buffer' => sub {
        my $self = THE_CLASS->_get_fresh_instance;
        my $buffers = _set_buffers($self, 'a', 'b');

        $self->buffer_push('some initialization');

        is_deeply $buffers, ['a', 'b', 'some initialization'], 'a new buffer was appended';
    };

    it 'defaults to a semicolon' => sub {
        my $self = THE_CLASS->_get_fresh_instance;
        my $buffers = _set_buffers($self, 'a', 'b');

        $self->buffer_push;

        is_deeply $buffers, ['a', 'b', '; '], 'a new buffer was appended';
    };
};

describe buffer_join => sub {
    it 'collapses the last two buffers' => sub {
        my $self = THE_CLASS->_get_fresh_instance;
        my $buffers = _set_buffers($self, 'a', 'b', 'c');

        $self->buffer_join;

        is_deeply $buffers, ['a', 'bc'], 'the last two buffers have been joined';
    };
};

describe buffer_write => sub {
    it 'appends data to the second-to-last buffer' => sub {
        my $self = THE_CLASS->_get_fresh_instance;
        my $buffers = _set_buffers($self, 'a', 'b', 'c', 'd');

        $self->buffer_write('some data');

        is_deeply $buffers, ['a', 'b', 'csome data', 'd'], 'data has been appended';
    };
};

describe slif_source => sub {
    it 'returns the single remaining buffer' => sub {
        my $self = THE_CLASS->_get_fresh_instance;
        my $buffers = _set_buffers($self, 'the contents');

        my $slif_source = $self->slif_source;

        is $slif_source, 'the contents', 'got the last buffer';
    };

    it 'throws if more than a single buffer is left' => sub {
        my $self = THE_CLASS->_get_fresh_instance;
        my $buffers = _set_buffers($self, 'a', 'b');

        throws_ok { $self->slif_source } qr/\AInline rules were not closed\b/, 'throws an error because there were two buffers';
    };
};

describe docs => sub {
    it 'returns the documentation hash' => sub {
        my $self = THE_CLASS->_get_fresh_instance;
        my $expected = $self->$__docs__;

        my $docs = $self->docs;
        is $docs, $expected, 'got the documentation';
    };
};

describe expect => sub {
    it 'returns the token if it has the expected type' => sub {
        my @tokens = (
            [THE_CLASS->IDENT => 'the value'],
        );
        my $self = Local::MockNextToken->new(mock_tokens => \@tokens);

        my @args = $self->expect(THE_CLASS->IDENT);

        is scalar @args, 1, 'got back one item';
        my ($value) = @args;
        is $value, 'the value', 'got expected value';
    };

    it 'accepts multiple possible types' => sub {
        my @tokens = (
            [THE_CLASS->IDENT => 'the value'],
        );
        my $self = Local::MockNextToken->new(mock_tokens => \@tokens);

        my @args = $self->expect(THE_CLASS->LITERAL, THE_CLASS->IDENT, THE_CLASS->OP);

        is scalar @args, 1, 'got back one item';
        my ($value) = @args;
        is $value, 'the value', 'got expected value';
    };

    it 'dies when the token type is not accepted' => sub {
        my @tokens = (
            [THE_CLASS->IDENT => 'the value'],
        );
        my $self = Local::MockNextToken->new(mock_tokens => \@tokens);

        throws_ok { $self->expect(THE_CLASS->LITERAL, THE_CLASS->OP) }
            qr/\A\Qexpected {LITERAL, OP} not IDENT\E\b/;
    };

    it 'dies if no tokens are available' => sub {
        my @tokens = (
            [],
        );
        my $self = Local::MockNextToken->new(mock_tokens => \@tokens);

        throws_ok { $self->expect(THE_CLASS->LITERAL, THE_CLASS->OP) }
            qr/\A\Qexpected {LITERAL, OP} but reached end of input\E/;
    };
};

describe next_token => sub {
    it 'returns empty when at end of input' => sub {
        case 'empty string' => sub {
            my $input = '';
            my $self = THE_CLASS->_get_fresh_instance(\$input);
            pos($input) = 0;

            my @result = $self->next_token;

            is_deeply \@result, [], 'returned empty';
        };

        case 'end of string' => sub {
            my $input = 'foo bar';
            my $self = THE_CLASS->_get_fresh_instance(\$input);
            pos($input) = length($input);

            my @result = $self->next_token;

            is_deeply \@result, [], 'returned empty';
        };
    };

    it 'appends whitespace to buffer, then returns next token' => sub {
        _::const my $WHITESPACE => '    ';

        case 'before end of string' => sub {
            my $input = $WHITESPACE;
            my $self = THE_CLASS->_get_fresh_instance(\$input);
            pos($input) = 0;
            my $buffers = _set_buffers($self, 'a', 'b');

            my @result = $self->next_token;

            is_deeply \@result, [], 'next token is EOS';
            is_deeply $buffers, ['a' . $WHITESPACE, 'b'], 'space was appended to buffer';
        };

        case 'before IDENT' => sub {
            my $input = $WHITESPACE . 'name';
            my $self = THE_CLASS->_get_fresh_instance(\$input);
            pos($input) = 0;
            my $buffers = _set_buffers($self, 'a', 'b');

            my @result = $self->next_token;

            is_deeply \@result, [THE_CLASS->IDENT => 'name'], 'next token is IDENT';
            is_deeply $buffers, ['a' . $WHITESPACE, 'b'], 'space was appended to buffer';
        };
    };

    it 'appends comments to buffer, then returns next token' => sub {
        _::const my $COMMENT => '# helpful description';

        case 'before end of string' => sub {
            my $input = $COMMENT;
            my $self = THE_CLASS->_get_fresh_instance(\$input);
            pos($input) = 0;
            my $buffers = _set_buffers($self, 'a', 'b');

            my @result = $self->next_token;

            is_deeply \@result, [], 'next token is EOS';
            is_deeply $buffers, ['a' . $COMMENT, 'b'], 'comment was appended to buffer';
        };

        case 'before IDENT' => sub {
            my $input = " $COMMENT\n name";
            my $self = THE_CLASS->_get_fresh_instance(\$input);
            pos($input) = 0;
            my $buffers = _set_buffers($self, 'a', 'b');

            my @result = $self->next_token;

            is_deeply \@result, [THE_CLASS->IDENT => 'name'], 'next token is IDENT';
            is_deeply $buffers, ["a $COMMENT\n ", 'b'], 'comment was appended to buffer';
        };
    };

    it 'returns simple identifiers' => sub {
        my $input = '****foo_bar****';
        my $self = THE_CLASS->_get_fresh_instance(\$input);
        pos($input) = 4;
        my $buffers = _set_buffers($self, 'a', 'b');

        my @result = $self->next_token;

        is_deeply \@result, [THE_CLASS->IDENT, 'foo_bar'], 'got IDENT';
        is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
        is pos($input), 4 + length 'foo_bar', 'match position advanced';
    };

    it 'returns namespaced identifiers' => sub {
        my $input = '****%Foo****';
        my $self = THE_CLASS->_get_fresh_instance(\$input);
        pos($input) = 4;
        my $buffers = _set_buffers($self, 'a', 'b');
        $self->$__namespace__('TheNamespace');

        my @result = $self->next_token;

        is_deeply \@result, [THE_CLASS->IDENT, 'TheNamespace' . THE_CLASS->NAMESPACE_SEPARATOR . 'Foo'], 'got namespaced IDENT';
        is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
        is pos($input), 4 + length '%Foo', 'match position advanced';
    };

    it 'returns namespace references' => sub {
        my $input = '****%****';
        my $self = THE_CLASS->_get_fresh_instance(\$input);
        pos($input) = 4;
        my $buffers = _set_buffers($self, 'a', 'b');
        $self->$__namespace__('TheNamespace');

        my @result = $self->next_token;

        is_deeply \@result, [THE_CLASS->IDENT, 'TheNamespace'], 'got the namespace';
        is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
        is pos($input), 4 + length '%', 'match position advanced';
    };

    it 'dies if no namespace was set' => sub {
        my $input = '****%****';
        my $self = THE_CLASS->_get_fresh_instance(\$input);
        pos($input) = 4;

        throws_ok { $self->next_token }
            qr/\A\QNo namespace was set\E\b/;
    };

    it 'returns literal strings' => sub {
        # actually ****'foo\\bar\'****
        my $input = q(****'foo\\\\bar\\'****);
        my $self = THE_CLASS->_get_fresh_instance(\$input);
        pos($input) = 4;
        my $buffers = _set_buffers($self, 'a', 'b');

        my @result = $self->next_token;

        is_deeply \@result, [THE_CLASS->LITERAL, q('foo\\\\bar\\')], 'got correct string';
        is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
        is pos($input), 4 + length q('foo\\\\bar\\'), 'match position advanced';
    };

    it 'returns literal strings with case insensitive modifier' => sub {
        # actually ****'foo\\bar\':i****
        my $input = q(****'foo\\\\bar\\':i****);
        my $self = THE_CLASS->_get_fresh_instance(\$input);
        pos($input) = 4;
        my $buffers = _set_buffers($self, 'a', 'b');

        my @result = $self->next_token;

        is_deeply \@result, [THE_CLASS->LITERAL, q('foo\\\\bar\\':i)], 'got correct string';
        is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
        is pos($input), 4 + length q('foo\\\\bar\\':i), 'match position advanced';
    };

    it 'returns character classes' => sub {
        # actually ****[^\w\\a-z\-\]\[{]]****
        my $input = q(****[^\\w\\\\a-z\\-\\]\\[{]****);
        my $self = THE_CLASS->_get_fresh_instance(\$input);
        pos($input) = 4;
        my $buffers = _set_buffers($self, 'a', 'b');

        my @result = $self->next_token;

        is_deeply \@result, [THE_CLASS->LITERAL, q([^\\w\\\\a-z\\-\\]\\[{])], 'got correct charclass';
        is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
        is pos($input), 4 + length q([^\\w\\\\a-z\\-\\]\\[{]), 'match position advanced';
    };

    it 'delegates to commands' => sub {
        package Local::MockTestCommand {
            use Moo;
            extends 'MarpaX::R2::GrammarPreprocessor';

            sub command_test {
                /\G \s* (foo)\b/xgc
                    or die "Expected foo";
                return __PACKAGE__->OP => $1 . '_bar';
            }
        }

        # actually ****\test foo****
        my $input = q(****\\test  foo****);
        my $self = Local::MockTestCommand->_get_fresh_instance(\$input);
        pos($input) = 4;
        my $buffers = _set_buffers($self, 'a', 'b');

        my @result = $self->next_token;

        is_deeply \@result, [THE_CLASS->OP, 'foo_bar'], 'got parse result';
        is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
        is pos($input), 4 + length q(-test  foo), 'match position advanced';
    };

    it 'throws for unknown commands' => sub {
        my $input = q(****\\this_command_does_not_exist_I_hope****);
        my $self = THE_CLASS->_get_fresh_instance(\$input);
        pos($input) = 4;

        throws_ok { $self->next_token }
            qr/\A\QUnknown command this_command_does_not_exist_I_hope, could not find command_this_command_does_not_exist_I_hope method\E\b/;
    };

    it 'recurses into inline rules' => sub {
        my $input = q(****{ name bleh, parser don't care' }****);
        my $self = THE_CLASS->_get_fresh_instance(\$input);
        pos($input) = 4;
        my $buffers = _set_buffers($self, 'a', 'b');

        my @result = $self->next_token;

        is_deeply \@result, [THE_CLASS->IDENT, 'name'], 'inline rule returns its identifier';
        is_deeply $buffers, ['a', q(b name bleh, parser don't care' ; )], 'rule body was appended to deferred buffer';
        is pos($input), 4 + length q({ name bleh, parser don't care' }), 'match position advanced';
    };

    it 'returns close tokens' => sub {
        my $input = q(****}****);
        my $self = THE_CLASS->_get_fresh_instance(\$input);
        pos($input) = 4;
        my $buffers = _set_buffers($self, 'a', 'b');

        my @result = $self->next_token;

        is_deeply \@result, [THE_CLASS->CLOSE, undef], 'got CLOSE token';
        is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
        is pos($input), 4 + 1, 'match position advanced';
    };

    it 'returns anything else as operators' => sub {
        my $input = q(****::=|+*=>glorp****);
        my $self = THE_CLASS->_get_fresh_instance(\$input);
        pos($input) = 4;
        my $buffers = _set_buffers($self, 'a', 'b');

        my @result = $self->next_token;

        is_deeply \@result, [THE_CLASS->OP, '::=|+*=>'], 'got OP token';
        is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
        is pos($input), 4 + length '::=|+*=>', 'match position advanced';
    };
};

describe command_array => sub {
    my $input = q(****++++);
    pos($input) = 4;
    my $self = THE_CLASS->_get_fresh_instance;
    my $buffers = _set_buffers($self, 'a', 'b');

    my @result = do {
        local *_ = \$input;
        $self->command_array;
    };

    is_deeply \@result, [THE_CLASS->OP, 'action => ::array'], 'got SLIF snippet';
    is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
    is pos($input), 4, 'match position unchanged';
};

describe command_null => sub {
    my $input = q(****++++);
    pos($input) = 4;
    my $self = THE_CLASS->_get_fresh_instance;
    my $buffers = _set_buffers($self, 'a', 'b');

    my @result = do {
        local *_ = \$input;
        $self->command_null;
    };

    is_deeply \@result, [THE_CLASS->OP, 'action => ::undef'], 'got SLIF snippet';
    is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
    is pos($input), 4, 'match position unchanged';
};

describe command_group => sub {
    my $input = q(****++++);
    pos($input) = 4;
    my $self = THE_CLASS->_get_fresh_instance;
    my $buffers = _set_buffers($self, 'a', 'b');

    my @result = do {
        local *_ = \$input;
        $self->command_group;
    };

    is_deeply \@result, [THE_CLASS->OP, 'assoc => group'], 'got SLIF snippet';
    is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
    is pos($input), 4, 'match position unchanged';
};

describe command_left => sub {
    my $input = q(****++++);
    pos($input) = 4;
    my $self = THE_CLASS->_get_fresh_instance;
    my $buffers = _set_buffers($self, 'a', 'b');

    my @result = do {
        local *_ = \$input;
        $self->command_left;
    };

    is_deeply \@result, [THE_CLASS->OP, 'assoc => left'], 'got SLIF snippet';
    is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
    is pos($input), 4, 'match position unchanged';
};

describe command_right => sub {
    my $input = q(****++++);
    pos($input) = 4;
    my $self = THE_CLASS->_get_fresh_instance;
    my $buffers = _set_buffers($self, 'a', 'b');

    my @result = do {
        local *_ = \$input;
        $self->command_right;
    };

    is_deeply \@result, [THE_CLASS->OP, 'assoc => right'], 'got SLIF snippet';
    is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
    is pos($input), 4, 'match position unchanged';
};

describe command_lax => sub {
    my $input = q(****++++);
    pos($input) = 4;
    my $self = THE_CLASS->_get_fresh_instance;
    my $buffers = _set_buffers($self, 'a', 'b');

    my @result = do {
        local *_ = \$input;
        $self->command_lax;
    };

    is_deeply \@result, [THE_CLASS->OP, 'proper => 0'], 'got SLIF snippet';
    is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
    is pos($input), 4, 'match position unchanged';
};

describe command_do => sub {
    my $input = q(**** FooBar++++);
    pos($input) = 4;
    my $self = THE_CLASS->_get_fresh_instance;
    my $buffers = _set_buffers($self, 'a', 'b');

    my @result = do {
        local *_ = \$input;
        $self->command_do;
    };

    is_deeply \@result, [THE_CLASS->OP, 'action => do_FooBar'], 'got SLIF snippet';
    is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
    is pos($input), 4 + length q( FooBar), 'match position advanced';
};

describe command_sep => sub {
    my @tokens = (
        [THE_CLASS->IDENT => 'some_identifier'],
        [THE_CLASS->OP => 'we should never see this'],
    );

    my $self = Local::MockNextToken->new(mock_tokens => \@tokens);
    my $buffers = _set_buffers($self, 'a', 'b');

    my @result = do {
        local *_ = \undef;
        $self->command_sep;
    };

    is_deeply \@result, [THE_CLASS->OP, 'separator => some_identifier'], 'got SLIF snippet';
    is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
};

describe command_keyword => sub {
    my @tokens = (
        [THE_CLASS->IDENT => 'some_identifier'],
        [THE_CLASS->OP => 'we should never see this'],
    );

    my $self = Local::MockNextToken->new(mock_tokens => \@tokens);
    my $buffers = _set_buffers($self, 'a', 'b');

    my @result = do {
        local *_ = \undef;
        $self->command_keyword;
    };

    is_deeply \@result, [THE_CLASS->IDENT, 'some_identifier'], 'passes through identifier';
    is_deeply $buffers, ['a:lexeme ~ some_identifier priority => 1;', 'b'], 'appends keyword prioritization to immediate buffer';
};

describe command_namespace => sub {
    my @tokens = (
        [THE_CLASS->IDENT => 'some_identifier'],
        [THE_CLASS->OP => 'we should never see this'],
    );

    my $self = Local::MockNextToken->new(mock_tokens => \@tokens);
    my $buffers = _set_buffers($self, 'a', 'b');
    $self->$__namespace__(undef);

    my @result = do {
        local *_ = \undef;
        $self->command_namespace;
    };

    is $self->$__namespace__, 'some_identifier', 'namespace has been set';
    is_deeply \@result, [THE_CLASS->IDENT, 'some_identifier'], 'passes through identifier';
    is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
};

describe command_doc => sub {
    my $input =
        qq(**** """ foo\n) .
        qq("""   bar\n) .
        qq(      """ baz\n) .
        qq( "" qux);
    pos($input) = 4;
    my @tokens = (
        [THE_CLASS->IDENT => 'some_identifier'],
        [THE_CLASS->OP => 'we should never see this'],
    );

    my $self = Local::MockNextToken->new(mock_tokens => \@tokens);
    my $buffers = _set_buffers($self, 'a', 'b');
    my $docs = $self->$__docs__({ foo => "foo docs"});

    my @result = do {
        local *_ = \$input;
        $self->command_doc;
    };

    is_deeply \@result, [THE_CLASS->IDENT, 'some_identifier'], 'passes through identifier';
    is_deeply $buffers, ['a', 'b'], 'buffers are unchanged';
    is_deeply $docs, {
        foo => "foo docs",
        some_identifier => "foo\n  bar\nbaz",
    }, 'stored documentation';
    is pos($input), length(qq(**** """ foo\n) . qq("""   bar\n) . qq(      """ baz\n)),
        'matcj position advanced';
};

done_testing;

sub _set_buffers {
    my ($self, @buffers) = @_;
    my $buffers = $self->$__buffers__;
    @$buffers = @buffers;
    return $buffers;
}
