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
        die pinpoint_error($grammar, $recce, $processed->docs, $input, "Parse failed");
    }

    return $$maybe_data;
}

BEGIN {
    package Actions;
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
        $docs =~ s/^/  /mg;
        s/\s*\z// for $docs, @childs;
        return $heading . main::tree_join($docs, @childs) . "\n";
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

    return pinpoint_error($grammar, $recce, $grammar_documentation, $source, $problem, $err);
}

sub pinpoint_error {
    my ($grammar, $recce, $grammar_documentation, $source, $problem, $orig_error) = @_;
    $orig_error //= $problem;

    # Find the location of the error
    my $position = $recce->pos;
    my ($line, $col) = $recce->line_column($position);

    # Extract the offending source line
    my $line_start = $position - $col + 1;
    pos($source) = $line_start;
    my ($line_contents) = $source =~ /\G (.*)/x
        or return $orig_error, "\n(line could not be extracted)";

    my $progress_report = progress_report($grammar, $recce, $grammar_documentation);

    # get info about the last successfuly parsed thing
    my $last_completed_report;
    if (my ($g1_start, $g1_length) = $recce->last_completed('Value')) {
        my @candidate_symbols;
        my $g1_end = $g1_start + $g1_length;
        my $STATUS_COMPLETION = -1;
        for my $progress_item (@{ $recce->progress($g1_end) }) {
            my ($rule_id, $status, $start) = @$progress_item;
            next if $status != $STATUS_COMPLETION;
            next if $start != $g1_start;  # get longest completions only

            my ($lhs_id, @rhs_ids) = $grammar->rule_expand($rule_id);
            my $symbol = $grammar->symbol_name($lhs_id);
            next if $symbol eq 'Value';  # only interested in the specific kind of value

            push @candidate_symbols, $symbol;
        }

        if (@candidate_symbols) {
            my $span = $recce->substring($g1_start, $g1_length);
            $span =~ s/\P{XPosixGraph}/ /g; # simplify input
            $last_completed_report = sprintf "Last recognized value: %s: %s\n",
                scalar oxford_comma_join('or', @candidate_symbols),
                elide_middle($span, 10, "...", 10);
        }
    }

    # Print nice error
    my $line_pointer = (q[ ] x ($col - 1)) . '^';
    my $offending_char_description = ($position < length $source)
        ? describe_char(substr($source, $position, 1))
        : "EOF (end of input)";
    return "Parser: $line:$col: error: $problem\n",
        $line_contents, "\n",
        $line_pointer, "\n",
        "at " . $offending_char_description . "\n",
        $last_completed_report // '',
        "Expected one of the following:\n",
        $progress_report;
}

sub progress_report {
    my ($grammar, $recce, $grammar_documentation) = @_;

    my $doclet_root = Local::Doclet->new(name => '<<ROOT>>',
                                         fullname => undef,
                                         doc_hash => $grammar_documentation);
    my $doclet_insert = sub {
        my ($fullname, $rhs) = @_;

        # skip if symbol is hidden
        return if is_hidden_documentation($grammar_documentation, $fullname);

        $doclet_root->insert($rhs, split /.\K__/, $fullname);
        return;
    };

    # Get expected lexemes
    $doclet_insert->($_, undef) for @{ $recce->terminals_expected };

    # \%raw_progress : { $lhs_id => { $location => [\@rhs] }, ... }
    my %raw_progress;
    for my $progress_spec (@{ $recce->progress() }) {
        my ($id, $location, $start) = @$progress_spec;
        my ($lhs_id, @rhs_ids) = $grammar->rule_expand($id);

        push @{ $raw_progress{$lhs_id}{$location} }, \@rhs_ids;
    }

    # delete those LHSes that are hidden from the doc system
    for my $lhs_id (keys %raw_progress) {
        delete $raw_progress{$lhs_id}
            if is_hidden_documentation($grammar_documentation, $grammar->symbol_name($lhs_id));
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

    return $progress_report;
}

#!! elide_middle($string: Str, $prefix: Size, $ellipsis: Str, $suffix: Size) -> Str
#!!
#!! Removes the middle bit of long strings.
#!!
#!! Example:
#!!
#!!     elide_middle("123foobarbaz", 3, "...", 3) #=> "123...baz"
sub elide_middle {
    my ($str, $prefix_size, $ellipsis, $suffix_size) = @_;
    # delete $prefix and $suffix, so $middle is left
    my $middle = $str;
    my $prefix = substr($middle, 0, $prefix_size, '');
    my $suffix = substr($middle, -$suffix_size, $suffix_size, '');
    # only remove the middle if it shortens the string
    if (length $middle > length $ellipsis) {
        return $prefix . $ellipsis . $suffix;
    }
    else {
        return $str;
    }
}

#!! oxford_comma_join($conjunction: Str, ...@words: Str) -> Str
#!!
#!! join words with commata, according to the rules of the English language.
#!!
#!! Example:
#!!
#!!     oxford_comma_join('and', qw(a)) #=> "a"
#!!     oxford_comma_join('and', qw(a b)) #=> "a and b"
#!!     oxford_comma_join('and', qw(a b c)) #=> "a, b, and c"
sub oxford_comma_join {
    my ($conjunction, @words) = @_;
    return if not @words;
    return $words[0] if @words == 1;
    return "$words[0] $conjunction $words[1]" if @words == 2;
    my $last_word = pop @words;
    return (join ', ', @words) . ", $conjunction $last_word";
}

#!! describe_char($char: Char) -> String
#!!
#!! Example:
#!!
#!!     describe_char("a") #=> "character 'a' U+0061 LATIN SMALL LETTER A"
sub describe_char {
    my ($char) = @_;
    require charnames;

    my $display_form = ($char =~ /\A\p{XPosixGraph}\z/) ? "character '$char'" : "unprintable character";
    my $codepoint = ord $char;
    my $name = sprintf 'U+%04X %s', $codepoint, charnames::viacode($codepoint);
    return "$display_form $name";
}

#!! is_hidden_documentation($docs: Docs,
#!!                         $symbol: SymbolName,
#!!                         ) -> Bool
#!! check if symbol was explicitly hidden
#!!
#!! @param $docs: Docs
#!! the preprocessed docs, where Docs = Hash[SymbolName, String?].
#!!
#!! @param $symbol: SymbolName
#!! the symbol to check for
#!!
#!! @returns Bool
#!! true when the symbol is known to the doc system and was explicitly hidden.
sub is_hidden_documentation {
    my ($grammar_documentation, $symbol) = @_;
    return (exists $grammar_documentation->{$symbol}
        and not defined $grammar_documentation->{$symbol});
}

#!! tree_join($cont: Str?, ...@childs: Str) -> Str
#!!
#!! create a pretty line art tree from the items
#!!
#!! Example:
#!!
#!!     tree_join("...cont", "foo", "bar\nbaz", "qux")
#!!
#!! produces:
#!!
#!!     │ ...cont
#!!     ├╴foo
#!!     ├╴bar
#!!     │ baz
#!!     └╴qux
#!!
#!! but
#!!
#!!     tree_join("...cont") #=> "  ...cont"
sub tree_join {
    my ($cont, @childs) = @_;

    @childs ? s/^/│ /mg : s/^/  /mg for $cont // ();

    if (@childs) {
        my $last_child = pop @childs;
        s/\A/├╴/, s/\n\K/├╴/g for @childs;
        s/\A/└╴/, s/\n\K/  /g for $last_child;
        push @childs, $last_child;
    }

    return join "\n", $cont // (), @childs;
}

__DATA__
# Naming convention:
# CamelCase – G1 LHS symbols
# ALL_UPPERCASE – G1 terminals / L0 top level symbols
# all_lowercase – L0 symbols

""" a JSON document, can either be an Object or an Array
\namespace Json
    ::= (_) %TopLevelItem (_)

    """ the top level of a JSON document must be an Object or Array
    %TopLevelItem ::= Object | Array

    """ any value
    Value ::= Object | Array | String | Number | Boolean | Null

    \doc hide
    unicorn ~ [^\s\S] # can never be lexed

""" optional white space
\namespace _
    ::= \doc hide \optional %Whitespace \null

    \doc hide
    %Whitespace ::= %Item+ \null

    \doc hide
    %Item
        ::= { \doc hide %WS ~ [\s]+ } \null
        |   ErrorComment \null

\doc hide
\namespace ErrorComment
    ::= %COMMENT_INTRO %ERROR

    \doc hide
    %COMMENT_INTRO ~ '//' | '/*' | '#'

    """ ERROR: JSON does not support comments!
    %ERROR ~ unicorn

""" either "true" or "false"
\namespace Boolean
    ::= {   \doc hide %true ~ 'true' } \do True
    |   {   \doc hide %false ~ 'false' } \do False

""" "null", the absence of values
\namespace Null
    ::= {   \doc hide %keyword ~ 'null' } \null

""" a key-value collection
\namespace Object
    ::= (%open _) %Contents (_ %close) \do Object

    """ zero or more %KeyValuePairs separated by Comma
    %Contents ::= %KeyValuePair* \sep Comma \array

    """ a key-value pair
    %KeyValuePair ::= String (_ %colon _) Value \array

    """ "{" begins an object
    %open ~ '{'

    """ "}" ends an object
    %close ~ '}'

    """ ":" separates keys from values
    %colon ~ ':'

""" a sequential collection
\namespace Array
    ::= (%open _) %Contents (_ %close)

    """ zero or more Values separated by Comma
    %Contents ::= Value* \sep Comma \array

    """ "[" begins an array
    %open ~ '['

    """ "]" ends an array
    %close ~ ']'

""" "," separates items in an Object or Array
\namespace Comma
    ::= (_  %comma  _) \null

    \doc hide
    %comma ~ ','

""" quoted text with various allowed escapes
\namespace String
    ::= (%quote) { \doc hide %Contents ::= %Item* \array } (%quote) \do String

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

""" integer or real number
\namespace Number
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
