use strict;
use warnings;
use utf8;

=pod

=encoding utf8

=head1 NAME

MarpaX::Grammar::Preprocessor::Result - a preprocessed SLIF grammar

=cut

package MarpaX::Grammar::Preprocessor::Result;
use Moo;

=head1 SYNOPSIS

    my $result = MarpaX::Grammar::Preprocessor->new->preprocess($source);

    my $slif = $result->slif_source;
    my $docs_for_foo = $result->docs->{foo};

=head1 DESCRIPTION

This class represents the preprocessor output.
Instances are returned by the C<preprocess()> method in
L<MarpaX::Grammar::Preprocessor|MarpaX::Grammar::Preprocessor>.
You should not create instances yourself.

=head1 METHODS

=head2 slif_source

    my $source = $result->slif_source;

Return the preprocessed SLIF source for use in a SLIF grammar.

B<Returns>
the preprocessed source as a string.
Note the the SLIF grammar constructor exoectes a reference to a string, not a plain string.

B<Throws> never.

=cut

has slif_source => (is => 'ro', required => 1);

=head2 docs

    my $docs = $result->docs;

Retrieves the symbol documentation hash table.

If a symbol exists as a key in the table,
then documentation was specified for that symbol.
If the value was undefined, the symbol was explicitly hidden from the documentation system.
Otherwise (for defined values), that value is the documentation string.

B<Returns>
the symbol documentation as a read-only hash reference.

B<Throws> never.

B<Stability:>
May add arguments in a backwards-compatible manner.

B<Example:>

    my $docs = $result->docs;

    # list the documentation for all @symbols
    for my $symbol (@symbols) {

        # if no docs were provided, use a placeholder documentation
        if (not exists $docs->{$symbol}) {
            say "$symbol: undocumented";
            next;
        }

        my $docs_for_this_symbol = $docs->{$symbol};

        # if the symbol was hidden, skip it
        next if not defined $docs_for_this_symbol;

        # output the documentation
        $docs_for_this_symbol =~ s/\n/\n    /g;  # indent all but the first lines
        say "$symbol: $docs_for_this_symbol";
    }

=cut

has docs => (is => 'ro', required => 1);

1;
