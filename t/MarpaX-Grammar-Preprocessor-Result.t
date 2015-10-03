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

use constant THE_CLASS => 'MarpaX::Grammar::Preprocessor::Result';

use_ok THE_CLASS;

{
    my $expected_slif_source = 'a';
    my $expected_docs = { a => 42 };

    my $self = THE_CLASS->new(
        slif_source => $expected_slif_source,
        docs => $expected_docs,
    );

    is $self->slif_source, $expected_slif_source, 'ctor sets slif_source';
    is $self->docs, $expected_docs, 'ctor sets docs';
}

done_testing;