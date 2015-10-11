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

use constant THE_CLASS => 'MarpaX::Grammar::Preprocessor';

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

describe TOKEN_TYPE => sub {
    is THE_CLASS->TOKEN_TYPE, 'MarpaX::Grammar::Preprocessor::TokenType', 'got the expected class';
};

describe constants => sub {
    for my $NAME (qw/IDENT LITERAL OP CLOSE/) {
        describe $NAME => sub {
            ok defined THE_CLASS->$NAME, 'has constant';
            is THE_CLASS->$NAME, $NAME, 'TokenType has correct name';
            is THE_CLASS->TOKEN_TYPE->coerce($NAME), THE_CLASS->$NAME, 'can look constant up by name';
        };
    }
};

BEGIN {
    package Local::ParserMock;
    use Moo;
    extends 'MarpaX::Grammar::Preprocessor';

    has source_ref => (is => 'ro', required => 1);

    use constant EXPECTED_SOURCE => 'the source';
    use constant EXPECTED_RESULT => 'some fascinating result';
    our $WAS_CALLED = 0;

    sub pump {
        my ($self) = @_;
        $WAS_CALLED = 1;
        my $source_ref = $self->source_ref;
        Test::More::is $$source_ref, EXPECTED_SOURCE, 'got the expected input source';
        return MarpaX::Grammar::Preprocessor->EOF;
    }

    sub result {
        my ($self) = @_;
        return EXPECTED_RESULT;
    }

    sub _factory {
        my (undef, $source_ref, %args) = @_;
        return __PACKAGE__->new(
            source_ref => $source_ref,
            %args,
        );
    }
}

describe preprocess => sub {
    it 'can take strings' => sub {
        local $Local::ParserMock::WAS_CALLED = 0;

        my $self = THE_CLASS->new(
            parser_factory => \&Local::ParserMock::_factory,
        );

        my $result = $self->preprocess(Local::ParserMock::EXPECTED_SOURCE);

        is $result, Local::ParserMock::EXPECTED_RESULT;
        ok $Local::ParserMock::WAS_CALLED, 'pump was called',
    };

    it 'can take file handles' => sub {
        local $Local::ParserMock::WAS_CALLED = 0;
        my $source_copy = Local::ParserMock::EXPECTED_SOURCE;
        open my $fh, "<", \$source_copy
            or die "Could not open PerlIO file handle: $!";

        my $self = THE_CLASS->new(
            parser_factory => \&Local::ParserMock::_factory,
        );

        my $result = $self->preprocess($fh);

        is $result, Local::ParserMock::EXPECTED_RESULT;
        ok $Local::ParserMock::WAS_CALLED, '_MarpaX_Grammar_Preprocessor_pump was called',
    };

    it 'dies when not invoked on an object' => sub {
        throws_ok { MarpaX::Grammar::Preprocessor->preprocess('foo bar') }
            qr/\A\Qexpected MarpaX::Grammar::Preprocessor instance in call to preprocess()\E/;
    };

    it 'dies when input wasn\'t fully consumed' => sub {
        my $self = THE_CLASS->new;

        throws_ok { $self->preprocess('foo } bar') }
            qr/\A\QUnexpected token CLOSE\E\b/;
    };
};

done_testing;
