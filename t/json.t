#!/usr/bin/env perl
use strict;
use warnings;
use utf8;

# This test is an example use case for the preprocessor,
# and heavily showcases usage of the documentation feature.
#
# You can run it as an ordinary script by providing any command line argument.
# A dash "-" reads from STDIN. E.g. run
#
#   perl json.t weird.json
#
# to have the script explain parse errors for you.
#
# Note how the parser clearly displays the location of the error,
# E.g. here the parser found a problem with an illegal string escape:
#
#     Parser: 1:28: error: Error in SLIF parse: No lexeme found at line 1, column 28
#     {"foo": 2.1E0, "bar": "baz\x65...
#                                ^
#     at character 'x'
#     we are trying to parse one of the following:
#     \namespace String
#     │   quoted text with various allowed escapes
#     ├╴%EscapeSequence -> · %newline_escape
#     │ %EscapeSequence -> · %backspace_escape
#     │ %EscapeSequence -> · %carriage_return_escape
#     │ %EscapeSequence -> · %tab_escape
#     │ %EscapeSequence -> · %form_feed_escape
#     │ %EscapeSequence -> · %escape_unicode
#     │ %EscapeSequence -> · %escaped_literal
#     │     ??? undocumented
#     ├╴%backspace_escape
#     │     "\b" backspace escape
#     ├╴%carriage_return_escape
#     │     "\r" carriage return escape
#     ├╴%escape_unicode
#     │     "\uXXXX" an unicode escape for a 16-bit surrogate half in hex notation, e.g. \u2603 for ☃
#     ├╴%escaped_literal
#     │     escaped literal characters: double quote '"', backslash "\", solidus "/"
#     ├╴%form_feed_escape
#     │     "\f" form feed escape
#     ├╴%newline_escape
#     │     "\n" newline escape
#     └╴%tab_escape
#           "\t" tab escape
#
# It is not perfect, but is a massive increase in user-friendliness and debuggability.

use MarpaX::Grammar::Preprocessor;
use Marpa::R2;

use Test::More;
use Test::Exception;

use Util::Underscore;

my $processed = MarpaX::Grammar::Preprocessor->preprocess(\*DATA);
my $grammar = Marpa::R2::Scanless::G->new({ source => \$processed->slif_source });

# enter script mode if any command line arguments were provided
if (@ARGV) {
    my $input = do { local $/; <> };
    _::dd json_load($input);
    exit;
}

# otherwise, enter test mode

{
    my $input  = '{"foo": 2.1E, "bar" ...';
    my $errptr = '            ^';
    throws_ok { json_load($input) }
        qr/\A.*\n\Q$input\E\n\Q$errptr\E\n/,
        'grammar requires full exponents';
}

{
    my $input  = '{"foo": 2.1E0, "bar": "baz\x65...';
    my $errptr = '                           ^';
    throws_ok { json_load($input) }
        qr/\A.*\n\Q$input\E\n\Q$errptr\E\n/,
        'detects invalid \\x escape';
}

{
    my $input  = '{"foo": 2.1E0, "bar": "baz\u0065"} // a JSON document';
    my $errptr = '                                     ^';
    throws_ok { json_load($input) }
        qr/\A.*\n\Q$input\E\n\Q$errptr\E\n/,
        'detects illegal comments';
}

{
    my $input  = '{"foo": 2.1E0, "bar": "baz\u0065"} ';
    is_deeply json_load($input), { foo => 2.1, bar => "baze" }, 'works';
}

{
    my $input  = ' [ -1.02E+03, {}, true, false, null ]';
    is_deeply json_load($input), [-1020, {}, 1, 0, undef], 'works';
}

done_testing;

sub json_load {
    my ($input) = @_;

    my $recce = Marpa::R2::Scanless::R->new({ grammar => $grammar });

    eval { $recce->read(\$input); 1 }
        or die pinpoint_marpa_error($grammar, $recce, $processed->docs, $input, $@);

    my $maybe_data = $recce->value(Actions->new);

    if (not $maybe_data) {
        die "Parse failed";
    }

    return $$maybe_data;
}

package Actions {
    use Moo;

    sub do_Object {
        my ($self, $items) = @_;
        my %hash;
        for my $item (@$items) {
            my ($key, $value) = @$item;
            $hash{$key} = $value;
        }
        return \%hash;
    }

    sub do_String {
        my ($self, $items) = @_;
        return join "", @$items;
    }

    sub do_StringEscape {
        my ($self, $letter) = @_;
        return { n => "\n", t => "\t", b => "\b", f => "\f", r => "\r" }->{$letter};
    }

    sub do_Number {
        my ($self, $sign, $integral, $fractional, $exponent) = @_;
        my $num = join '', ($sign // ''), ($integral),  @{ $fractional // []}, grep defined, @{$exponent // []};
        return 0+$num;
    }

    sub do_StringUnicodeEscape {
        my ($self, $uhex) = @_;
        my $hex = $uhex =~ s/^u//r;
        return chr hex $hex;
    }

    sub do_True { 1 }
    sub do_False { 0 }
}

BEGIN {
    package Local::Doclet;
    use Moo;

    has childs  => (
        is => 'ro',
        default => sub { {} });

    has name => (
        is => 'ro',
        required => 1);

    has fullname => (
        is => 'ro',
        required => 1);

    has rhses => (
        is => 'ro',
        default => sub { [] });

    has doc_hash => (
        is => 'ro',
        required => 1);

    sub get_or_add_child {
        my ($self, $name) = @_;
        my $fullname = $self->fullname;
        if (defined $fullname) {
            $fullname .= '__' . $name;
        }
        else {
            $fullname = $name;
        }
        return $self->childs->{$name}
            //= $self->new(name => $name,
                           fullname => $fullname,
                           doc_hash => $self->doc_hash);
    }

    sub docs {
        my ($self) = @_;
        my $key = $self->fullname;
        my $docs = $self->doc_hash;
        return '??? undocumented' if not exists $docs->{$key};
        return $docs->{$key} // '';
    }

    sub push_rhses {
        my ($self, @rhses) = @_;
        push @{ $self->rhses }, @rhses;
    }

    sub insert {
        my ($self, $rhses, @path) = @_;
        if (@path) {
            my $name = shift @path;
            return $self->get_or_add_child($name)->insert($rhses, @path);
        }

        $self->push_rhses(@$rhses) if defined $rhses;
        return;
    }

    sub render_childs {
        my ($self, $prefix) = @_;
        my $childs = $self->childs;
        return map { $childs->{$_}->render($prefix) } sort keys %$childs;
    }

    sub render {
        my ($self, $prefix) = @_;
        my $childs_hash = $self->childs;

        my @childs = $self->render_childs('%');

        my $name = $self->name;

        my $prefixed_name = (length $prefix) ? $prefix . $name : $name;
        my $heading = "\\namespace $prefixed_name\n";
        my $rhses = $self->rhses;
        if (@$rhses) {
            $heading = '';
            my $namespace = (@childs) ? $self->fullname : $self->fullname =~ s/__\Q$name\E\z//r;
            for my $rhs (@$rhses) {
                my @namespaced_rhs = map { s/\A\Q$namespace\E__/%/r } @$rhs;
                $heading .= sprintf "%s -> %s\n", $prefixed_name, join ' ', @namespaced_rhs;
            }
        }
        elsif (not @childs) {
            $heading = "$prefixed_name\n";
        }
        my $docs = $self->docs;
        if (length $docs) {
            if (@childs) {
                $docs =~ s/^/│   /mg;
            }
            else {
                $docs =~ s/^/    /mg;
            }
            $heading .= $docs;
            $heading =~ s/\s*\z/\n/;
        }

        my $out = $heading;

        while (my ($i, $rendering) = each @childs) {
            $rendering =~ s/\s*\z//;
            if ($i < $#childs) {
                $rendering =~ s/\n\K/│ /g;
                $rendering =~ s/\A/├╴/;
            }
            else {
                $rendering =~ s/\n\K/  /g;
                $rendering =~ s/\A/└╴/;
            }
            $out .= $rendering . "\n";
        }

        return $out;
    }
}

sub pinpoint_marpa_error {
    my ($grammar, $recce, $grammar_documentation, $source, $err) = @_;

    # Check that we actually have a Marpa exception
    $err =~ /\n\QMarpa::R2 exception at \E/
        or return $err;

    # Try to extract the error message
    my ($problem) = $err =~ /^(.*)\n\Q* String before error: \E/s
        or return $err, "\n(Marpa error format not recognized)";
    $problem =~ s/^error:\s*//i;
    $problem =~ s/\s*\z//;

    # Find the location of the error
    my $position = $recce->pos;
    my ($line, $col) = $recce->line_column($position);

    # Extract the offending source line
    my $line_start = $position - $col + 1;
    pos($source) = $line_start;
    my ($line_contents) = $source =~ /\G (.*)/x
        or return $err, "\n(line could not be extracted)";

    my $doclet_root = Local::Doclet->new(name => '<<ROOT>>',
                                         fullname => undef,
                                         doc_hash => $grammar_documentation);
    my $doclet_insert = sub {
        my ($fullname, $rhs) = @_;

        # skip if symbol is hidden
        return if exists $grammar_documentation->{$fullname}
            and not defined $grammar_documentation->{$fullname};

        $doclet_root->insert($rhs, split /.\K__/, $fullname);
        return;
    };

    # Get expected lexemes
    $doclet_insert->($_, undef) for @{ $recce->terminals_expected };

    my %raw_progress; # { $lhs_id => { $location => [\@rhs] }, ... }
    for my $progress_spec (@{ $recce->progress }) {
        my ($id, $location, $start) = @$progress_spec;
        my ($lhs_id, @rhs_ids) = $grammar->rule_expand($id);

        # ignore all rules with this LHS,
        my $docsymbol = $grammar->symbol_name($lhs_id);
        next if exists $grammar_documentation->{$docsymbol}
            and not defined $grammar_documentation->{$docsymbol};

        push @{ $raw_progress{$lhs_id}{$location} }, \@rhs_ids;
    }

    # iterate over the raw progress, alphabetically sorted by LHS name
    for my $lhs_id (keys %raw_progress) {
        my $lhs_symbol = $grammar->symbol_display_form($lhs_id);
        my $locations = $raw_progress{$lhs_id};

        # iterate over all locations
        my @rhs_strings;
        for my $location (sort { $a <=> $b } keys %$locations) {
            my $all_rhs_ids = $locations->{$location};
            for my $rhs_ids (@$all_rhs_ids) {
                my @rhs_symbols = map { $grammar->symbol_display_form($_) } @$rhs_ids;
                s/__Optional\z/\?/g for @rhs_symbols;  # shorten optionals
                splice @rhs_symbols, $location, 0, '·';
                push @rhs_strings, [@rhs_symbols];
            }
        }

        $doclet_insert->($lhs_symbol, \@rhs_strings);
    }

    # create progress report
    my $progress_report = join '', $doclet_root->render_childs;
    $progress_report = "  none\n" if not length $progress_report;

    # Print nice error
    my $line_pointer = (q[ ] x ($col - 1)) . '^';
    require charnames;
    my $offending_char = substr($source, $position, 1);
    my $offending_char_description = ($offending_char =~ /\A\p{XPosixGraph}\z/) ? "character '$offending_char'" : 'unprintable character';
    $offending_char_description .= sprintf ' U+%04X %s', ord $offending_char, charnames::viacode(ord $offending_char);
    return "Parser: $line:$col: error: $problem\n",
        $line_contents, "\n",
        $line_pointer, "\n",
        "at " . $offending_char_description . "\n",
        "we are trying to parse one of the following:\n",
        $progress_report;
}

__DATA__
\namespace
    """ a JSON document, can either be an Object or an Array
    Json ::= (_) { \doc hide %TopLevelItem ::= Object | Array } (_)

    """ any value
    Value ::= Object | Array | String | Number | Boolean | Null

    \doc hide
    unicorn ~ [^\s\S] # can never be lexed

\namespace
    """ optional white space
    _ ::= %Item* \null
    \doc hide
    %Item
        ::= { \doc hide %WS ~ [\s]+ } \null
        |   ErrorComment \null

\namespace
    \doc hide
    ErrorComment ::= %COMMENT_INTRO %ERROR

    \doc hide
    %COMMENT_INTRO ~ '//' | '/*' | '#'

    """ ERROR: JSON does not support comments!
    %ERROR ~ unicorn

\namespace
    """ either "true" or "false"
    Boolean
        ::= {   """ a "true" boolean
                %true ~ 'true' } \do True
        |   {   """ a "false" boolean
                %false ~ 'false' } \do False

\namespace
    """ "null", the absence of values
    Null
        ::= {   """ "null", the absence of values
                %keyword ~ 'null' } \null

\namespace
    """ a key-value collection
    Object ::= (%open _) { \doc hide %Contents ::= %KeyValuePair* \sep Sep \array } (_ %close) \do Object

    """ a key-value pair
    %KeyValuePair ::= String (_ %colon _) Value \array

    """ "{" begins an object
    %open ~ '{'

    """ "}" ends an object
    %close ~ '}'

    """ ":" separates keys from values
    %colon ~ ':'

\namespace
    """ a sequential collection
    Array ::= (%open _) { \doc hide %Contents ::= Value* \sep Sep \array } (_ %close)

    """ "[" begins an array
    %open ~ '['

    """ "]" ends an array
    %close ~ ']'

\namespace
    """ separates items in an object or list
    Sep ::= (_  %comma  _) \null

    """ separates items in an object or list
    %comma ~ ','

\namespace
    """ quoted text with various allowed escapes
    String ::= (%quote) { \doc hide %Contents ::= %Item* \array } (%quote) \do String

    """ '"' begins and ends a string
    %quote ~ '"'

    \doc hide
    %Item
        ::= %ordinary_string_contents
        |   (%backslash) %EscapeSequence

    """ anything but a quote or backslash
    %ordinary_string_contents ~ [^"\\]+

    """ "\" begins an escape sequence
    %backslash ~ '\'

    \doc hide
    %EscapeSequence
    ::= {   """ "\n" newline escape
            %newline_escape ~ 'n' } \do StringEscape
    |   {   """ "\b" backspace escape
            %backspace_escape ~ 'b' } \do StringEscape
    |   {   """ "\r" carriage return escape
            %carriage_return_escape ~ 'r' } \do StringEscape
    |   {   """ "\t" tab escape
            %tab_escape ~ 't' } \do StringEscape
    |   {   """ "\f" form feed escape
            %form_feed_escape ~ 'f' } \do StringEscape
    |   {   """ "\uXXXX" an unicode escape for a 16-bit surrogate half in hex notation, e.g. \u2603 for ☃
            %escape_unicode ~ 'u' %xdigit %xdigit %xdigit %xdigit } \do StringUnicodeEscape
    |   {   """ escaped literal characters: double quote '"', backslash "\", solidus "/"
            %escaped_literal ~ [\\"/] }

    \doc hide
    %xdigit ~ [\p{PosixXDigit}]

\namespace
    """ integer or real number
    Number
        ::= \doc hide
            \optional { """ "-" starts a negative number
                        %minus ~ '-'}
            %LEADING_DIGITS
            \doc hide \optional %FractionalPart
            \doc hide \optional %Exponent
            \do Number

    """ The fractional part of a number, e.g. ".0230"
    %FractionalPart
        ::= {  """ a period "." separates the integral part from the fractional part of a number
                %PERIOD ~ '.' }
            %DIGITS
            \array

    """ The exponent of a number, e.g. "E-02".
    %Exponent
        ::= {   """ The exponent separator "e" or "E" separates the exponent from the rest of the number
                %EXPONENT_SEPARATOR ~ [eE] }
            \doc hide
            \optional { """ The exponent can be positive "+" or negative "-", defaulting to positive
                        %EXPONENT_SIGN ~ [+-] }
            %DIGITS
            \array


    """ an arbitrary sequence of decimal digits
    %DIGITS ~ [0-9]+

    """ the integral part of a number, e.g. "0", "7", "23"
    %LEADING_DIGITS ~ '0' | [1-9] { %leading_digits_rest ~ [0-9]* }
