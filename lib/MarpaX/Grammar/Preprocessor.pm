use strict;
use warnings;
use utf8;

=pod

=encoding utf8

=cut

################################################################################
#
# NAME
#
################################################################################

=head1 NAME

MarpaX::Grammar::Preprocessor - Shortcuts for Marpa::R2 SLIF grammars

=cut

package MarpaX::Grammar::Preprocessor;

use Util::Underscore v1.3.0;

################################################################################
#
# VERSION
#
################################################################################

=head1 VERSION

v0.0_1

=cut

use version 0.77; our $VERSION = qv('v0.0_1');

################################################################################
#
# SYNOPSIS
#
################################################################################

=head1 SYNOPSIS

    use MarpaX::Grammar::Preprocessor;
    use Marpa::R2;

    my $preprocessed = MarpaX::Grammar::Preprocessor->preprocess(\*DATA)
    my $grammar = Marpa::R2::Scanless::G->new({ source => \$preprocessed->slif_source });

    __DATA__
    # Everything that's legal in the SLIF DSL is also legal here

    # We automatically get this prelude:
    #   inaccessible is fatal by default
    #   :default ::= action => ::first
    #   lexeme default = latm => 1

    # A namespace allows us to gensym names for %helper rules
    \namespace Foo ::= %BAR | %Baz
    %BAR ~ 'bar'        # really Foo__BAR
    %Baz ::= %BAR %BAR  # really Foo__Baz

    # The next namespace is totally unrelated.
    \namespace
    Qux ::= %BAR
    %BAR ~ 'quxbar'     # really Qux__BAR

    # Associate a docstring with the next symbol.
    # Docstrings can span multiple lines, all beginning with a triple quote.
    \namespace
    \doc""" a list of values. Examples:
        """     []          (empty list)
        """     [1, 2, 3]   (list with three integers)
    List ::= (LEFT_BRACKET) %Items (RIGHT_BRACKET)
    %Items ::= Value* \sep COMMA  # \sep expands to "separator => "

    # Use { curly braces } to specify an inline rule.
    # Inline rules still need a name.
    # \array expands to "action => ::array"
    \namespace
    \doc""" a key-value dictionary
    Dict ::= (LEFT_BRACE) { %Items ::= %KVItem* \sep COMMA \array } (RIGHT_BRACE)
    %KVItem ::= Key (COLON) Value \array

    # Suppress documentation for any symbol.
    # Great for internal helper rules!
    \doc hide
    Shhh ~ 'no one can see me'

    # Easily link action rules with \do
    Action ::= Stations \do Action # expands to "action => do_Action"

=cut

################################################################################
#
# DESCRIPTION
#
################################################################################

=head1 DESCRIPTION

This module is a preprocessor for SLIF grammars.
Any valid SLIF grammar should be passed through with no modifications,
except for the prelude that is added.

This preprocessor is fairly restricted and mostly only does local,
token-based substitutions, similar to the C preprocessor.
The inline rule feature is more advanced,
but still operates on a source level.
Please keep this low-level approach in mind when using the module
– it does not build an AST for the SLIF, and does not use Marpa itself.

=head2 Commands

The most prominent feature are the commands.
These are introduced by a backslash,
and call back to Perl code which may do custom parsing
or use a simple token-based system to process the SLIF grammar.
Example: C<\lax \sep COMMA> is transformed to C<< proper => 0 separator => COMMA >>.

See L<COMMANDS|/"COMMANDS"> for detailed documentation on available commands.

=head2 Namespaces

Frequently, we need simple helper rules
that are of no concern for the rest of the grammar.
This preprocessor can mark any identifier as the current C<\namespace>.
Any plain C<%> (percent symbol) is then used as a reference to the current namespace,
and any C<%name> (name prefixed with percent symbol) is prefixed with the current namespace.
This makes it easy to have quasi-private names without too much typing.

Example:

    \namespace
    Term ::= % (%PLUS) Factor
    %PLUS ~ '+'

Is transformed to:

    Term ::= Term (Term__PLUS) Factor
    Term__PLUS ~ '+'

The namespace separator is currently set to double underscores,
so you shouldn't use them in your identifiers
(see also the stabilit policy section).

=head2 Inline Rules

Many rules in a SLIF grammar are only used in one place,
and are due to SLIF restrictions.
E.g. a sequence rule must be a rule of its own.
This preprocessor allows you to specify rules inline at their point of usage.
Unfortunately, they still need a name.
The preprocessor will then replace the inline rule with its symbol,
and defer the definition of the rule until a safe state is reached.

Example:

    \namespace
    List ::= ('[') { %Items ::= Value* \sep COMMA \array } (']')

Is transformed to:

    List ::= ('[') List__Items (']')
    List__Items ::= Value* separator => COMMA action => ::array


=head2 Docstrings

The C<\doc> commands lets you annotate a symbol with a docstring.
After processing, these docstrings are available as a name to docstring hash ref.
They can be used to build fairly sophisticated help systems that display relevant information on parse errors.

Example:

    \doc""" A list contains a sequence of zero or more values.
        """ It must start and end with square brackets,
        """ and contains comma-separated values. Example:
        """
        """    []      # an empty list
        """    [1]     # list with single element
        """    [1,]    # trailing comma is allowd
        """    [1,2,3] # list with three values
        """    [ 1 , 2 , 3 ]   # space within the array is ignored
    List ::= ...

This documentation could then be used to display a help message like this to the user:

    test.foo:42:4: error: no token found
        (1, 2, 3)
        ^
    at character '(' U+0028 LEFT PARENTHESIS

    expected:
      - List: A list contains a sequence of zero or more values.
        It must start and end with square brackets,
        and contains comma-separated values. Example:

            []      # an empty list
            [1]     # list with single element
            [1,]    # trailing comma is allowd
            [1,2,3] # list with three values
            [ 1 , 2 , 3 ]   # space within the array is ignored
      - Dict: A dict contains any number of key-value pairs
        ...
    ...

This module does not include such a help system!
However, you can see the test case C<t/json.t>
for a sample implementation of such “intelligent” error messages.
Run it as a script and pass it faulty input to see it in action.

=head2 Custom Commands

You can add custom commands by subclassing this module
and adding a C<command_FooBar()> method for a C<\FooBar> command.
No additional registration has to be done.
Since this module uses the Moo object system, I recommend you use it as well.

When your command is encountered in the input, the processor will invoke that method.
The SLIF source will be available in the C<$_> variable,
and C<pos()> will be set to the current position.
You can therefore use an C<m/\G ... /gc> style match to do your own parsing.
Alternatively, you can L<C<expect()>|/"expect"> a certain token type,
or poll for the L<C<next_token()>|/"next_token"> regardless of type.
Please see their reference documentation for more details.

The command must return a list of two values: the token type and the token value.
See L<C<TOKEN_TYPES>|/"TOKEN_TYPES"> for a list of valid token types.
Pick an appropriate token type depending on how that value might be used.
E.g. if the return value is to be used on the right hand side of a rule,
it must be an C<IDENT> or C<LITERAL>.

You may also use the
L<C<buffer_push()>|/"buffer_push">,
L<C<buffer_write()>|/"buffer_write">, and
L<C<buffer_join()>|/"buffer_join"> methods.
The buffer system manages any number of buffers.
One buffer is the currently selected buffer, another is the deferred buffer.
Only write to the current buffer if you know it is safe to do so
(e.g. if your command is supposed to be only used at the start of a rule).
Otherwise, select the deferred buffer with C<buffer_write()>.
Once finished, you will need to switch back with C<buffer_join()>.
The contents of the deferred buffer will be appended to the main buffer
once that buffer is complete and in a presumably syntactically safe state.

Actually, buffers are way less complicated than that – just read the source of this module.

=cut

################################################################################
#
# ATTRIBUTES
#
################################################################################

use Moo 2;
use Lexical::Accessor 0.008;

# the currently active namespace, or undef if no namespace is set.
lexical_has namespace   => (accessor => \my $NAMESPACE);

# the documentation hash mapping symbols to docstrings
lexical_has docs        => (accessor => \my $DOCS);

# the input grammar
lexical_has source_ref  => (accessor => \my $SOURCE_REF);

# the output buffers.
# Manipulate this only through the buffer_* methods!
lexical_has buffers     => (accessor => \my $BUFFERS);

# default-initialize the lexical attributes
# since Moo can't do it for us :(
sub BUILD {
    my ($self) = @_;
    $self->$NAMESPACE(undef);
    $self->$DOCS({});
    $self->$BUFFERS([$self->SLIF_PRELUDE]); # prime the first buffer
    return;
}

# DO NOT USE – only for testing
# Subject to change without any notice
#
# Break encapsulation so that we can write white-box tests
sub _MarpaX_Grammar_Preprocessor_test_accessors {
    my ($self, $name) = @_;
    return +{
        namespace   => $NAMESPACE,
        docs        => $DOCS,
        source_ref  => $SOURCE_REF,
        buffers     => $BUFFERS,
    }->{$name};
}

################################################################################
#
# METHODS
#
################################################################################

=head1 METHODS

This section lists general methods,
and methods that can be called on the result object.

For methods that are only valid during parsing, see L<METHODS DURING PARSING|/"METHODS DURING PARSING">.

=head2 preprocess

    my $preprocessed = MarpaX::GrammarPreprocessor->preprocess($source);

Processes the SLIF source.

See the DESCRIPTION section for an overview of the accepted language.

B<$source>
is the input to be processed.
It can either be a string, or an open file handle.

B<Returns>
a MarpaX::GrammarPreprocessor object in completed state.
You will probably want to call C<slif_source> on it.

B<Throws>
unspecified errors on illegal input strings.

B<Stability:>
May add arguments in a backwards-compatible manner.

B<Example:>

    my $preprocessed = MarpaX::GrammarPreprocessor->preprocess(\*DATA);

    my $grammar = Marpa::R2::Scanless::G->new({ source => \$preprocessed->slif_source });

    ... # parse something with the grammar

    __DATA__
    ...  # an extened SLIF grammar

=cut

sub preprocess {
    my ($class, $source) = @_;

    $source = do { local $/; <$source> } if _::is_open $source;
    pos($source) = 0;

    my $self = $class->_MarpaX_Grammar_Preprocessor_get_fresh_instance(\$source);

    $self->_MarpaX_Grammar_Preprocessor_pump;

    return $self;
}

# DO NOT USE – internal method
#
# create and initialize a new instance
#
# Unfortunately, there's no good way to initialize lexical accessors from a
# constructor, so we wrap the ctor instead.
sub _MarpaX_Grammar_Preprocessor_get_fresh_instance {
    my ($class, $source_ref) = @_;

    my $self = $class->new;

    $self->$SOURCE_REF($source_ref);

    return $self;
}

# DO NOT USE – internal method
#
# poll for tokens until source is exhausted
#
# This pumps the whole input through the preprocessor.
sub _MarpaX_Grammar_Preprocessor_pump {
    my ($self) = @_;

    # pump the whole source through the preprocessor
    $self->buffer_push;
    while (my ($type, $value) = $self->next_token) {
        $self->buffer_write($value);
    }
    $self->buffer_join;

    return;
}

=head2 slif_source

    my $source = $preprocessed->slif_source;

Retrieves the preprocessed SLIF source for use in a SLIF grammar.

B<Returns>
the preprocessed source as a string.
Note that the SLIF grammar constructor expects a reference to a string, not a plain string.

B<Throws>
if the input source was not syntactically correct,
e.g. if you forgot to close an inline rule.

B<Stability:>
Unlikely to change.

=cut

sub slif_source {
    my ($self) = @_;
    my $buffers = $self->$BUFFERS;

    die "Inline rules were not closed" if @$buffers != 1;

    return $buffers->[0];
}

=head2 docs

    my $docs = $preprocessed->docs;

Retrieves the symbol documentation hash table.

If a symbol exists as a key in the table,
then documentation was specific for that symbol.
If the value is undefined, the symbol was explicitly hidden for the documentation system,
Otherwise (for defined values) the value is a documentation string,

B<Returns:>
the symbol documentation as a read-only hash reference.

B<Throws>
never.

B<Stability:>
May add arguments in a backwards-compatible manner.

B<Example:>

    my $docs = $preprocessed->docs;

    # list the documentation for all @symbols
    my $output = '';
    for my $symbol (@symbols) {

        # If no documentation was provided,
        # list the symbol but use a placeholder for the docs
        if (not exists $docs->{$symbol}) {
            $output .= "  - $symbol: ???\n";
            next;
        }

        # If the symbol was hidden, skip it
        if (not defined $docs->{$symbol}) {
            next;
        }

        # output the documentation
        my $docs_for_this_symbol = $docs->{$symbol};
        $docs_for_this_symbol =~ s/\n/\n    /g; # indent the docs
        $output .= "  - $symbol: $docs\n";
    }

=cut

sub docs {
    my ($self) = @_;
    return $self->$DOCS;
}

=head2 TOKEN_TYPES

    my $ident_type = $self->TOKEN_TYPES->{'IDENT'};

    $self->IDENT
    $self->LITERAL
    $self->OP
    $self->CLOSE

Maps token type names to their IDs.
Available as named constants and as a hash ref.

The hash ref is more useful than the actual constants
if the token type name is not known at compile time.

B<IDENT>
is the token type ID for identifiers.

An identifier is any symbol that can occur on the left hand side of a rule definition.

B<LITERAL>
is the token type ID for literals.

A literal is any terminal on the right hand side of a rule,
e.g. strings or character classes.

B<OP>
is the token type ID for non-rule parts of the SLIF source.

This token is used for SLIF operators, adverbs,
or anything else that can't be classed as an IDENT or LITERAL.

B<CLOSE>
is the token type ID for closing scopes.

This token signals that an inline rule is coming to an end.

B<Returns>
a hash ref that maps token type names to their values.

B<Throws>
never.

B<Stability:>
Unlikely to change its interface.
The exact IDs are unspecified.

=cut

# token types
use constant TOKEN_TYPES => {
    IDENT   => 1,
    LITERAL => 2,
    OP      => 3,
    CLOSE   => 4,
};

# defines IDENT => 1, etc.
# think "use constant { IDENT => 1, ... };"
use constant TOKEN_TYPES;

=head2 TOKEN_NAMES

    my $name = $self->TOKEN_NAMES->{$token_id};

Maps token IDs to their names.

Given a token ID, this hash ref can retrieve the name of the token,
e.g. for better diagnostics.
This should be preferred over printing out the unspecified token ID.

B<Returns>
A hash reference with token type IDs as keys and token type names as values.

B<Throws>
never.

B<Stability:>
Unlikely to change its interface.
The exact entries in the hash are unspecified,
but it should contain an entry for each token ID that can be retrieved
through the constants or through the C<TOKEN_TYPES> map.

=cut

# reverse the TOKEN_TYPES
# Also serves as a check for 1:1 name:id mapping
use constant TOKEN_NAMES => do {
    my %reversed;
    while (my ($name, $id) = each %{TOKEN_TYPES()}) {
        die sprintf "redefined id %d from %s to %s", $id, $reversed{$id}, $name
            if exists $reversed{$id};
        $reversed{$id} = $name;
    }
    \%reversed;
};

=head2 NAMESPACE_SEPARATOR

The namespace separator for namespaced identifiers.

In the namespace C<Foo>, the identifier C<%Bar> would be translated to C<Foo__Bar>.
This constant can be overridden in a child class to choose a different namespace separator.

B<Returns>
the namespace separator as a string.
Defaults to a double underscore.

B<Throws>
never.

B<Stability:>
May be overridden in child classes.

B<Example:>

    package MyPreprocessor;
    use Moo;
    extends 'MarpaX::GrammarPreprocessor';

    use constant NAMESPACE_SEPARATOR => '666';

    ...
    # now identifier "%Bar" in namespace "Foo"
    # would be output as "Foo666Bar" in the SLIF

=cut

# \namespace Foo ... %Bar -> "Foo{{NAMESPACE_SEPARATOR}}Bar"
use constant NAMESPACE_SEPARATOR => '__';

=head2 SLIF_PRELUDE

    my $prelude = $self->SLIF_PRELUDE;

The definitions that are prepended to each preprocessed grammar.

This constant can be overridden in a child class to specify a different prelude.

The defaults will make inaccessible symbols illegal – they are an indication you forgot to complete your grammar.
It will also set a default rule action that always returns the first right-hand side value of a rule.
This is what you want if a rule only contains a single symbol on the right hand side,
but can lead to hard-to find bugs if you have more than one right-hand side symbol.
The default will also activate longest acceptable token matching, which is what you'd almost always want.

B<Returns>
the default prelude as a syntactically complete string.

B<Throws>
never.

B<Stability:>
May be overriden in child classes,

B<Example:>

    package MyPreprocessor;
    use 'Moo';
    extends 'MarpaX::GrammarPreprocessor';

    use constant SLIF_PRELUDE => ''; # no implicit prelude;

=cut

# stuffed before each grammar
use constant SLIF_PRELUDE =>
        'inaccessible is fatal by default ' .
        ':default ::= action => ::first ' .
        'lexeme default = latm => 1 ';

################################################################################
#
# METHODS DURING PARSING
#
################################################################################

=head1 METHODS DURING PARSING

=head2 buffer_push

    $self->buffer_push;
    $self->buffer_push($deferred_buffer_initial_value);

Selects the deferred buffer as the current buffer, and adds a new deferred buffer.

Given the current buffer "A" and the deferred buffer "B", this creates a new buffer "C".
After the call, the current buffer will be "B" and the deferred buffer "C".

You will need to call "buffer_join" afterwards to switch back to the original buffers.

B<$deferred_buffer_initial_value>
is the optional default value for the newly created buffer as a string.
If it is not provided, the buffer is initialized with unspecified syntax
that is safe to occur between SLIF rules,
e.g. a space or a semicolon.

B<Returns>
nothing.

B<Throws>
never.

B<Stability:>
This method is not expected to change.

B<Example:>

    $self->buffer_push;
    $self->buffer_write("HelperRule ::= Thing; ");
    $self->buffer_join;

=cut

sub buffer_push {
    my ($self, $init) = @_;
    push @{ $self->$BUFFERS }, $init // '; ';
    return;
}

=head2 buffer_write

    $self->buffer_write($slif_fragment);

Appends the given C<$slif_fragment> to the currently selected buffer.

You should make sure that the SLIF fragment
is valid in the context of the current buffer.
If the SLIF fragment does not necessarily have to be right here
but just at some point in the output,
then you can switch to the deferred buffer with C<buffer_push()>.

Note that writing to the buffer is an uncomposable side effect.
If your command only produces a single value,
you should probably use it as a return value
to allow other commands to apply additional transformations.
The preprocessor will eventually write any returned value to the buffer.

B<$slif_fragment>
is the string you want to write into the current buffer.

B<Returns>
nothing.

B<Throws>
if the currently selected buffer is not available
(e.g. if you said C<buffer_join()> once too much,
or if you didn't construct the parser via C<preprocess()>).

B<Stability:>
Not likely to change.

B<Example:>

    $self->buffer_write("# frobnication of the next symbol\n");

=cut

sub buffer_write {
    my ($self, @things) = @_;

    if (@{ $self->$BUFFERS } < 2) {
        die "No currently selected buffer";
    }

    $self->$BUFFERS->[-2] .= join '', @things;
    return;
}

=head2 buffer_join

    $self->buffer_join;

Appends the deferred buffer to the selected buffer, and selects the previous buffer.

Any call to C<buffer_push()> should be mirrored by a C<buffer_join()> call.

B<Returns>
nothing

B<Throws>
if no buffer is currently selected.

B<Stability:>
This function is not expected to change.

B<Example:>

    $self->buffer_push;
    $self->buffer_write("deferred stuff");
    $self->buffer_join;

=cut

sub buffer_join {
    my ($self) = @_;
    my $last_buffer = pop @{ $self->$BUFFERS };
    $self->$BUFFERS->[-1] .= $last_buffer;
    return;
}

=head2 next_token

    my ($type, $value) = $self->next_token;

Retrieves the next token from the input, applying special commands as needed.

This method is the main parsing driver.
It implements various special syntax
and delegates to C<command_*()> functions when a command is encountered.
Note that whitespace and comments
are directly written to the currently selected buffer
rather than returning them as a token.

B<Returns>
a two-value list:
B<$type> is the token type ID of the found token.
This should be only used for comparisons with the token type constants.
To get the name of the token type, use C<< $self->TOKEN_NAMES->{$type} >>.
B<$value> is the arbitrary value of the token,
which in general would be a string.

B<Throws>
on illegal syntax.

B<Stability:>
The interface of this method is not expected to change,
though new behaviour might be added in a backwards-compatible manner.

B<Example:>

    my ($type, $value) = $self->next_token;
    if ($type != $type->IDENT) {
        die "expected IDENT";
    }

=cut

sub next_token {
    my ($self) = @_;
    local *_ = $self->$SOURCE_REF;
    TRY: while (pos() < length) {
        if (m/\G( \s+ )/xgc) {
            $self->buffer_write($1);
            next TRY;
        }
        if (m/\G( [#] [^\n]* )/xgc) {
            $self->buffer_write($1);
            next TRY;
        }
        return IDENT, $1 if m/\G( [\w]+ )/xgc;
        if (m/\G [%]/xgc) {
            my $ns = $self->$NAMESPACE;
            die "No namespace was set" if not defined $ns;
            return IDENT, $ns . $self->NAMESPACE_SEPARATOR . $1 if m/\G ([\w]+) /xgc;
            return IDENT, $ns;
        }
        return LITERAL, $1 if m/\G( ['] [^']+ ['] (?: [:]i )? )/xgc;
        return LITERAL, $1 if m/\G( [\[] (?: [^\]\\]++ | [\\]. )+ [\]] )/xgc;
        if (m/\G [\\] (\w+)/xgc) {
            my $cmd = $1;
            my $command = $self->can("command_$cmd")
                or die "Unknown command $cmd, could not find command_$cmd method";
            return $command->($self);
        }
        if (m/\G [{]/xgc) {
            $self->buffer_push;
            my $name = $self->expect(IDENT);
            $self->buffer_write($name);
            while (my ($type, $value) = $self->next_token) {
                last if $type == CLOSE;
                $self->buffer_write($value);
            }
            $self->buffer_join;
            return IDENT, $name;
        }
        return CLOSE,   undef   if m/\G [\}]/xgc;
        return OP,      $1      if m/\G( [^\s\#\w\%\'\[\\]+ )/xgc;
        die "Unknown grammar str '" . (substr $_, pos, 20) . "...'";
    }
    return;
}

=head2 expect

    my $value = $self->expect(@expected_types);

Fetch the value of the C<next_token()> and assert that it is one of the expected C<TOKEN_TYPES>.

This function is useful for straightforward token-based custom parsing of the source.
You tell it what types of tokens you expected, and either get the value of a matching token,
or an error message.

B<@expected_types>
is a list of expected token types.

B<Returns>
the value of a matching token.

B<Throws>
when the next token does not match any of the C<@expected_types>.
The error message will explain the cause and list the expected token types.
Also throws when C<next_token()> would throw here.

B<Stability:>
not expected to change.

B<Example:>

    sub command_sep {
        my ($self) = @_;
        my $sep = $self->expect($self->IDENT, $self->LITERAL);
        return $self->OP, "separator => $sep";
    }

=cut

sub expect {
    my ($self, @expected) = @_;
    my ($type, $value) = $self->next_token
        or die sprintf "expected {%s} but reached end of input", (join ', ', @{TOKEN_NAMES()}{@expected});
    $type == $_ and return $value for @expected;
    die sprintf "expected {%s} not %s", (join ', ', @{TOKEN_NAMES()}{@expected}), TOKEN_NAMES->{$type};
}

################################################################################
#
# COMMANDS
#
################################################################################

=head1 COMMANDS

This section contains reference documentation for all preprocessor commands.
For a general overview of the available syntax, see the L<DESCRIPTION section|/"DESCRIPTION"> instead.

=head2 command \array

    \array

B<Expands to:>

    action => ::array

B<Example:>

    Items ::= Item* \array

=cut

sub command_array { return OP, 'action => ::array' }

=head2 command \null

    \null

B<Expands to:>

    action => ::undef

B<Example:>

    IgnoreMyValue ::= Foo Bar \null

=cut

sub command_null { return OP, 'action => ::undef' }

=head2 command \group

    \group

B<Expands to:>

    assoc => group

B<Example:>

    Expression
    ::= ('(') Expression (')') \group
    ||  ...

=cut

sub command_group { return OP, 'assoc => group' }

=head2 command \left

    \left

B<Expands to:>

    assoc => left

B<Example:>

    Expression
    ::= Expression ('+') Expression \left
    ||  ...

=cut

sub command_left { return OP, 'assoc => left' }

=head2 command \right

    \right

B<Expands to:>

    assoc => right

B<Example:>

    Expression
    ::= Expression ('^') Expression \right
    ||  ...

=cut

sub command_right { return OP, 'assoc => right' }

=head2 command \lax

    \lax

B<Expands to:>

    proper => 0

Example:

    List ::= Item* \lax \sep COMMA \array

=cut

sub command_lax { return OP, 'proper => 0' }

=head2 command \do

    \do PERL_BAREWORD

Specify a custom action as a Perl subroutine,
assuming that the sub name begins with C<do_>.

B<PERL_BAREWORD>
is some legal subroutine name.

B<Expands to:>

    action => do_PERL_BAREWORD

B<Example:>

    Expression
    ::= Expression ('+') Expression \do Addition
    ||  ...

=cut

sub command_do {
    m/\G \s+ (\w+) /xgc
        or die "Expected action name";
    return OP, "action => do_$1";
}

=head2 command \sep

    \sep RULE

Specify a separator for a sequence rule

B<RULE>
is an C<IDENT> or C<LITERAL> token.

B<Expands to:>

    separator => RULE

B<Example:>

    List ::= Item* \sep COMMA

=cut

sub command_sep {
    my ($self) = @_;
    my $sep = $self->expect(IDENT, LITERAL);
    return OP, "separator => $sep";
}

=head2 command \keyword

    \keyword IDENT

Specify a high priority for a lexeme C<IDENT>.
To be used at a lexeme definition.

B<IDENT>
is an C<IDENT> token.
This symbol name must refer to a lexeme.

B<Expands to> the C<IDENT> token.

B<Inserts:>

    :lexeme ~ IDENT priority => 1

B<Example:>

    \keyword KW_FOR ~ 'for'

=cut

sub command_keyword {
    my ($self) = @_;
    my $name = $self->expect(IDENT);
    $self->buffer_write(":lexeme ~ $name priority => 1;");
    return IDENT, $name;
}

=head2 command \namespace

    \namespace IDENT

Set the current namespace context,
so that it can be used for C<%> namespace references
or C<%name> internal rules.

B<IDENT>
is an C<IDENT> token.

B<Expands to> the C<IDENT> token,

B<Example:>

    \namespace
    Expression
    ::= % ('+') % \do Addition
    ||  ...

    \namespace
    List ::= ('[') %Items (']')
    %Items ::= Item* \sep COMMA

=cut

sub command_namespace {
    my ($self) = @_;
    my $name = $self->expect(IDENT);
    $self->$NAMESPACE($name);
    return IDENT, $name;
}

=head2 command \doc

    \doc hide IDENT
    \doc DOCSTRING IDENT

Associate a C<DOCSTRING> with a given C<IDENT>,
or C<hide> a symbol from the doc system.

You can retrieve the documentation hash via L<C<docs()>|/"docs"> from the result object.

B<IDENT>
is the symbol you want to document.

B<DOCSTRING> is at least one docstring line,
Each line starts with a triple quote and a space,
and then runs until the end of the line.

B<Expands to> the C<IDENT> token.

B<Example:>

    \doc hide
    InternalSymbol ::= ...

    \doc
    """ a sequence of zero or more values
    List ::= Value* \sep COMMA \array

    \doc
    """ a key-value list
    """
    """ Each key-value pair is separated by a colon. Example:
    """
    """     "foo": 42
    Dict ::= KVPair* \sep COMMA \array
    KVPair ::= String (COLON) Value \array

=cut

sub command_doc {
    my ($self) = @_;
    my $docs;
    if (m/\G \s* hide/xgc) {
        $docs = undef
    }
    elsif (m/\G \s* ["]["]["]\ ([^\n]* (?: \n | \z ))/xgc) {
        do {
            $docs .= $1
        } while (m/\G \s* ["]["]["]\ ([^\n]* (?: \n | \z ))/xgc);
        $docs =~ s/\s*\z//;
    }
    else {
        my $next_source = substr $_, pos, 20;
        die "Expected docstring near ", _::pp $next_source, "...";
    }

    my $symbol = $self->expect(IDENT);
    die "Symbol $symbol already documented" if exists $self->$DOCS->{$symbol};
    $self->$DOCS->{$symbol} = $docs;
    return IDENT, $symbol;
}

1;

__END__

################################################################################
#
# STATUS OF THIS MODULE/STABILITY POLICY
#
################################################################################

=head1 STATUS OF THIS MODULE/STABILITY POLICY

This module is reasonably complete
and is not expected to change much once it sees its v1.0 release.
Until then some changes may occur, but I don't expect dependent code to break.
After v1.0, any release that breaks documentented behaviour will increment the major version number.

Each documented method is annotated with an individual expected stability.

Since this module serves as a preprocessor for SLIF grammars,
all valid SLIF grammars should be passed through without modifications
(if not, that's a bug).

The CPAN namespace C<MarpaX::Grammar::Preprocessor::*> is reserved by this module for future use.
If you want to upload a module in this namespace (which might be reasonable for extensions),
then please discuss this with the L<author|/"AUTHOR"> first.
Maybe your changes could be patched into this module instead.
If not, I could at least place a link to your extension in this documentation.

=head2 Extending this module

This module is written with the expectation that it might be subclassed to provide new commands.
It uses Moo, and so should you.

You may add your own commands as explained in the Custom Commands section.
However, all custom commands must begin with an uppercase letter
(conversely, all builtin commands are guaranteed to always start with a lowercase letter).

To reserve room for expansion, subclasses may not add new methods, unless they
(a) are new commands using the C<command_NameStartsWithUppercase> naming scheme, or
(b) start with at least one underscore.
If a method starts with underscores,
it may not use the C<_MarpaX_Grammar_Preprocessor> prefix.

Additionally, some methods may be overridden:

=over

=item *
L<NAMESPACE_SEPARATOR|/"NAMESPACE_SEPARATOR">

=item *
L<SLIF_PRELUDE|/"SLIF_PRELUDE">

=back

=cut

################################################################################
#
# BUGS
#
################################################################################

=head1 BUGS

Please report any bugs to L<https://github.com/latk/p5-MarpaX-Grammar-Preprocessor/issues>.
If you file a bug, please try to include the following information if you are able to do so:

=over

=item *
your version of Perl: C<perl --version>

=item *
your version of Marpa: C<< perl -MMarpa::R2 -E'say Marpa::R2->VERSION' >>

=item *
your version of this module; C<< perl -MMarpaX::Grammar::Preprocessor -E'say MarpaX::Grammar::Preprocessor->VERSION' >>

=item *
explain what you did to trigger the bug

=item *
explain what you expected to happen

=item *
should you have experience with Perl testing:
Write a test case can use to reproduce and investigate the bug.
It should fail in the current state and pass when the bug was fixed,
so that it can be used as a regression test.

=back

Pull requests are also welcome.

=cut

################################################################################
#
# AUTHOR
#
################################################################################

=head1 AUTHOR

Lukas Atkinson (cpan: AMON) <amon@cpan.org>

=cut

################################################################################
#
# COPYRIGHT AND LICENSE
#
################################################################################

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2015 by Lukas Atkinson.

This is free software; you can redistribute it and/or modify it under the same terms as the Perl 5 programming language system itself.
