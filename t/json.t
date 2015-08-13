#!/usr/bin/env perl

# This test is an example use case for the preprocessor,
# and heavily showcases usage of the documentation feature.
#
# Note how the parser clearly displays the location of the error,
# If you run this code without the test but rather,
# you will get a detailed listing of expected symbols and their documentation.
# E.g. for the error in the string:
#
#   t/json.t ........................ 1/? Parser: 1:28: error: Error in SLIF parse: No lexeme found at line 1, column 28
#   {"foo": 2.1E0, "bar": "baz\x65...
#                              ^
#   at character 'x'
#   expected terminals:
#     - String__ESCAPED_LITERAL: escaped literal characters: double quote, backslash, solidus
#     - String__ESCAPE_CODE: escape codes:
#       \b : backspace
#       \n : newline
#       \r : carriage return
#       \t : horizontal tab
#     - String__ESCAPE_UNICODE: an unicode escape for a 16-bit surrogate half in hex notation, e.g. \u2603 for ☃
#   the parser thinks we are trying to parse one of the following:
#     none
#
# It is not perfect, but is a massive increase in user-friendliness and debuggability.

use strict;
use warnings;

use MarpaX::R2::GrammarPreprocessor;
use Marpa::R2;

use Test::More;
use Test::Exception;

my $processed = MarpaX::R2::GrammarPreprocessor->preprocess(\*DATA);
my $grammar = Marpa::R2::Scanless::G->new({ source => \$processed->slif_source });

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

    # Get expected lexemes
    my @terminals_expected = @{ $recce->terminals_expected };
    my $terminals_expected_report = '';
    for my $symbol (@terminals_expected) {
        my $documentation =
            (exists $grammar_documentation->{$symbol})
            ? $grammar_documentation->{$symbol}
            : '??? undocumented';
        next if not defined $documentation;
        $documentation =~ s/\n/\n    /g;
        $terminals_expected_report .= sprintf "  - %s: %s\n", $symbol, $documentation;
    }
    $terminals_expected_report ||= "  none\n";

    # Get completed rules
    my @completed_rules = _::uniq map { $grammar->rule_name($_->[0]) } @{ $recce->progress() };
    my $completed_rules_report = '';
    for my $symbol (@completed_rules) {
        my $documentation =
            (exists $grammar_documentation->{$symbol})
            ? $grammar_documentation->{$symbol}
            : '??? undocumented';
        next if not defined $documentation;
        $documentation =~ s/\n/\n    /g;
        $completed_rules_report .= sprintf "  - %s: %s\n", $symbol, $documentation;
    }
    $completed_rules_report ||= "  none\n";

    # Print nice error
    my $line_pointer = (q[ ] x ($col - 1)) . '^';
    return "Parser: $line:$col: error: $problem\n",
        $line_contents, "\n",
        $line_pointer, "\n",
        "at character '" . substr($source, $position, 1) . "'\n",
        "expected terminals:\n",
        $terminals_expected_report,
        "the parser thinks we are trying to parse one of the following:\n",
        $completed_rules_report;
}

__DATA__
\namespace
    \doc """ a JSON document, can either be an Object or an Array
    Json ::= (_) { \doc hide %TopLevelItem ::= Object | Array } (_)

    \doc """ any value
    Value ::= Object | Array | String | Number | Boolean | Null

    \doc hide
    NULL ::= \null # an empty placeholder

    \doc hide
    unicorn ~ [^\s\S] # can never be lexed

\namespace
    \doc """ optional white space
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

    \doc """ ERROR: JSON does not support comments!
    %ERROR ~ unicorn

\namespace
    \doc """ either "true" or "false"
    Boolean
        ::= {   \doc """ a "true" boolean
                %TRUE ~ 'true' } \do True
        |   {   \doc """ a "false" boolean
                %FALSE ~ 'false' } \do False

\namespace
    \doc """ "null", the absence of values
    Null
        ::= {   \doc """ "null", the absence of values
                %NULL ~ 'null' } \null

\namespace
    \doc """ a key-value collection
    Object ::= (OP_LBRACE _) { \doc hide %Items ::= %Item* \sep Sep \array } (_ OP_RBRACE) \do Object

    \doc """ a key-value pair
    %Item ::= String (_ OP_COLON _) Value \array

\namespace
    \doc """ a sequential collection
    Array ::= (OP_LBRACKET _) { \doc hide %Items ::= Value* \sep Sep \array } (_ OP_RBRACKET)

\namespace
    \doc """ separates items in an object or list
    Sep ::= (_) OP_COMMA (_) \null

\namespace
    \doc """ quoted text with various allowed escapes
    String ::= (OP_DQ) { \doc hide %Items ::= %Item* \array } (OP_DQ) \do String

    \doc hide
    %Item
        ::= {   \doc """ normal characters in a string: anything but a quote or backslash
                %RUN ~ [^"\\]+ }
        |   (%BACKSLASH) {
                \doc hide
                %Escape
                ::= {   \doc """ escaped literal characters: double quote, backslash, solidus
                        %ESCAPED_LITERAL ~ [\\"/] }
                |   {   \doc""" escape codes:
                            """ \b : backspace
                            """ \n : newline
                            """ \r : carriage return
                            """ \t : horizontal tab
                        %ESCAPE_CODE ~ [bfnrt] } \do StringEscape
                |   {   \doc """ an unicode escape for a 16-bit surrogate half in hex notation, e.g. \u2603 for ☃
                        %ESCAPE_UNICODE ~ 'u' %xdigit %xdigit %xdigit %xdigit
                        \doc hide
                        %xdigit ~ [\p{PosixXDigit}]
                    } \do StringUnicodeEscape
            }
    \doc """ the backslash is the escape character in strings
    %BACKSLASH ~ '\'

\namespace
    \doc """ integer or real number
    Number
        ::= {   \doc """ A Number may start with an optional minus
                %OptionalSign
                    ::= NULL
                    |   {   \doc """ "-" starts a negative number
                            %MINUS ~ '-'} }
            %LEADING_DIGITS
            { \doc hide %OptionalFractional ::= NULL | %FractionalPart }
            { \doc hide %OptionalExponent ::= NULL | %Exponent }
            \do Number

    \doc """ The fractional part of a number, e.g. ".0230"
    %FractionalPart
        ::= {  \doc """ a period "." separates the integral part from the fractional part of a number
                %PERIOD ~ '.' }
            %DIGITS
            \array

    \doc """ The exponent of a number, e.g. "E-02".
    %Exponent
        ::= {   \doc """ The exponent separator "e" or "E" separates the exponent from the rest of the number
                %EXPONENT_SEPARATOR ~ [eE] }
            {   \doc """ The exponent can be positive "+" or negative "-", defaulting to positive
                %OptionalExponentSign
                    ::= NULL
                    |   {   \doc """ The exponent can be positive "+" or negative "-", defaulting to positive
                            %EXPONENT_SIGN ~ [+-] } }
            %DIGITS
            \array


    \doc """ an arbitrary sequence of decimal digits
    %DIGITS ~ [0-9]+

    \doc """ the integral part of a number, e.g. "0", "7", "23"
    %LEADING_DIGITS ~ '0' | [1-9] { %leading_digits_rest ~ [0-9]* }

\doc """ ":", separates key-value items
OP_COLON ~ ':'

\doc """ "," separates items in a list or object
OP_COMMA ~ ','

\doc """ '"' starts and ends a string
OP_DQ ~ '"'

\doc """ "{" begins an object
OP_LBRACE ~ '{'

\doc """ "}" ends an object
OP_RBRACE ~ '}'

\doc """ "[" begins an array
OP_LBRACKET ~ '['

\doc """ "]" ends an array
OP_RBRACKET ~ ']'
