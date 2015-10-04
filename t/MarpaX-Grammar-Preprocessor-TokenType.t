#!/usr/bin/env perl
use strict;
use warnings;
use utf8;

use Test::More 1;
use Test::Exception;
use Util::Underscore v1.3.0;

use constant TOKEN_TYPE => 'MarpaX::Grammar::Preprocessor::TokenType';

use_ok TOKEN_TYPE;

sub describe {
    my ($name, $body) = @_;
    return subtest "describe $name", $body;
}

sub it {
    my ($name, $body) = @_;
    return subtest "it $name", $body;
}

my $TEST_TOKEN_TYPE_NAME = '__TEST_TOKEN_TYPE';

describe 'constructor' => sub {
    it 'can register a new type' => sub {
        ok +(_::none { "$_" eq $TEST_TOKEN_TYPE_NAME } TOKEN_TYPE->all_token_types),
            'the token type does not already exist';

        my $token = TOKEN_TYPE->new($TEST_TOKEN_TYPE_NAME);

        is $token->name, $TEST_TOKEN_TYPE_NAME, 'name() gives us the name';
        ok +(_::any { "$_" eq $TEST_TOKEN_TYPE_NAME } TOKEN_TYPE->all_token_types),
            'the new token type was registered';
    };

    it 'throws when the type was already defined' => sub {
        throws_ok { TOKEN_TYPE->new($TEST_TOKEN_TYPE_NAME) }
            qr/\A\Qcannot redefine $TEST_TOKEN_TYPE_NAME\E\b/;
    };
};

describe 'coerce' => sub {
    my $new_tt = TOKEN_TYPE->new('__TEST_TOKEN_TYPE2');

    it 'returns a TokenType instance' => sub {
        my $result = TOKEN_TYPE->coerce($new_tt);

        is $result, $new_tt, 'got back same object';
    };

    it 'maps strings to instances' => sub {
        my $result = TOKEN_TYPE->coerce('__TEST_TOKEN_TYPE2');

        is $result, $new_tt, 'got back instance from mapping';
    };

    it 'fails when name was not registered' => sub {
        throws_ok { TOKEN_TYPE->coerce('__TEST_TOKEN_TYPE_NONEXISTANT') }
            qr/\A\QNo token type for name=__TEST_TOKEN_TYPE_NONEXISTANT\E\b/;
    };

    it 'fails when name is undef' => sub {
        throws_ok { TOKEN_TYPE->coerce(undef) }
            qr/\A\QToken type must be TokenType instance or plain string\E\b/;
    };
};

describe 'operator ""' => sub {
    my $tt = TOKEN_TYPE->coerce($TEST_TOKEN_TYPE_NAME);
    is "$tt", $TEST_TOKEN_TYPE_NAME, 'stringification returns the name';
};

describe 'operator ==' => sub {
    my $tt = TOKEN_TYPE->coerce($TEST_TOKEN_TYPE_NAME);
    ok $tt == $tt, 'has identity';
    ok $tt == $TEST_TOKEN_TYPE_NAME, 'coerces name on the right';
    ok $TEST_TOKEN_TYPE_NAME == $tt, 'coerces name on the left';
    ok $tt != '__TEST_TOKEN_TYPE2', 'fails on wrong string right';
    ok '__TEST_TOKEN_TYPE2' != $tt, 'fails on wrong string left';
};

describe 'operator eq' => sub {
    my $tt = TOKEN_TYPE->coerce($TEST_TOKEN_TYPE_NAME);
    ok $tt eq $tt, 'has identity';
    ok $tt eq $TEST_TOKEN_TYPE_NAME, 'coerces name on the right';
    ok $TEST_TOKEN_TYPE_NAME eq $tt, 'coerces name on the left';
    ok $tt ne '__TEST_TOKEN_TYPE2', 'fails on wrong string right';
    ok '__TEST_TOKEN_TYPE2' ne $tt, 'fails on wrong string left';
};

describe 'builtin token types' => sub {
    is TOKEN_TYPE->coerce($_), $_
        for qw/IDENT LITERAL OP CLOSE/;
};

done_testing;