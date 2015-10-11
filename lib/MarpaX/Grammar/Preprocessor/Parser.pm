use strict;
use warnings;
use utf8;

=pod

=encoding utf8

=head1 NAME

MarpaX::Grammar::Preprocessor::Parser - parse extended SLIF grammars

=cut

package MarpaX::Grammar::Preprocessor::Parser;
use Moo;

use MarpaX::Grammar::Preprocessor::Result;
use MarpaX::Grammar::Preprocessor::TokenType;
use Util::Underscore v1.3.0;
use namespace::clean;

my $TOKEN_TYPE = 'MarpaX::Grammar::Preprocessor::TokenType';
my $TT_IDENT = $TOKEN_TYPE->coerce('IDENT');
my $TT_LITERAL = $TOKEN_TYPE->coerce('LITERAL');
my $TT_OP = $TOKEN_TYPE->coerce('OP');
my $TT_CLOSE = $TOKEN_TYPE->coerce('CLOSE');
my $TT_EOF = $TOKEN_TYPE->coerce('EOF');

=head1 SYNOPSIS

    # the parser is used created and used by "preprocess()"
    my $result = MarpaX::Grammar::Preprocessor->new->preprocess($source);

    # == internal methods ==

    my $source = "x ::= y";
    my $parser = MarpaX::Grammar::Preprocessor::Parser->new(
        source_ref => \$source,
        buffers => ['prelude; '],
        namespace_separator => '__',
    );
    $parser->pump;
    my $result = $parser->result;

    $parser->write('foo');
    $parser->write_deferred('foo ::= bar');

    my ($type, $value) = $parser->next_token;

    my $value = $parser->expect('IDENT', 'LITERAL');

=head2 CONSTRUCTOR

    $parser = MarpaX::Grammar::Preprocessor::Parser->new(
        source_ref => \$string,
        buffer => do { my $buffer = $prelude; \$buffer },
        buffer_deferred = do { my $buffer = ''; \$buffer },
        buffers => [$prelude],
        namespace_separator => '__',
    );

instantiate a parser.

B<source_ref>: scalar reference of string

The C<source_ref> named argument is the input SLIF source to the preprocessor.
The contents of the string will not be modified,
but the current match position C<pos($string)> will be changed.

B<buffer>: reference to string

B<buffer_deferred>: reference to string

The output buffers for the parser.
The referred-to string will be modified,
so you should probably pass a reference to a copy of any initial string
C<do { my $buffer = $initial; \$buffer }>
rather than passing a reference to a variable
C<\$initial>.


B<namespace_separator>: string

The separator for namespaced identifiers, defaults to C<__>.

In the naamespace C<Foo>, the identifier C<%Bar> would be translated to C<Foo__Bar>.
You can provide a different separator that is legal in SLIF rule names,
but doing so may break some code,

B<Returns>
a new Parser instance.

=cut

my %CTOR_ARG_MAPPING;

my $attribute = sub {
    my ($name, %args) = @_;

    if (my $init_arg = $args{init_arg}) {
        $CTOR_ARG_MAPPING{$init_arg} = $name;
    }
    else {
        $CTOR_ARG_MAPPING{$name} = $name;
    }

    has $name => (is => 'ro', %args);

    return sub { shift()->$name(@_) };
};

# the currently active namespace, or undef if no namespace is set
my $_namespace = $attribute->(
    _MarpaX_Grammar_Preprocessor_Parser_namespace =>
    is => 'rw',
    default => sub { undef },
);

# the thing between namespace parts
my $_namespace_separator = $attribute->(
    _MarpaX_Grammar_Preprocessor_Parser_namespace_separator =>
    init_arg => 'namespace_separator',
    default => sub { '__' },
);

# the documentation hash mapping symbols to docstrings
my $_docs = $attribute->(
    _MarpaX_Grammar_Preprocessor_Parser_docs =>
    default => sub { {} },
);

# the input grammar
my $_source_ref = $attribute->(
    _MarpaX_Grammar_Preprocessor_Parser_source_ref =>
    init_arg => 'source_ref',
    required => 1,
);

# the main output buffer
# Manipulate this only through the write() method.
my $_buffer = $attribute->(
    _MarpaX_Grammar_Preprocessor_Parser_buffer =>
    init_arg => 'buffer',
    required => 1,
);

# the deferred output buffer
# Manipulate this only through the write_deferred() method.
my $_buffer_deferred = $attribute->(
    _MarpaX_Grammar_Preprocessor_Parser_buffer_deferred =>
    init_arg => 'buffer_deferred',
    required => 1,
);

# avoids generating the same optional rule twice
my $_optional_rule_cache = $attribute->(
    _MarpaX_Grammar_Preprocessor_Parser_optional_rule_cache =>
    default => sub { {} },
);

################################################################################
#
# METHODS
#
################################################################################

=head2 METHODS

=head2 result

    my $result = $parser->result;

get the parse result.

B<Returns> a L<MarpaX::Grammar::Preprocessor::Result|MarpaX::Grammar::Preprocessor::Result> instance.

B<Throws> if the parse failed.

=cut

sub result {
    my ($self) = @_;

    my $buffer = $self->$_buffer;
    my $buffer_deferred = $self->$_buffer_deferred;
    my $docs = $self->$_docs;

    return MarpaX::Grammar::Preprocessor::Result->new(
        slif_source => $$buffer . $$buffer_deferred,
        docs => $docs,
    );
}

=head2 new_with

    $copy = $parser->new_with(%constructor_arguments);

creates a shallow copy of the C<$parser> with the given C<%constructor_arguments>.

The constructor for the C<$copy> will be invoked with the current state of the C<$parser>,
unless an argument is explicitly overridden.
So invoking C<new_with()> and not specifying any arguments will result in a shallow copy of the C<$parser>.

See the C<CONSTRUCTOR|/"CONSTRUCTOR"> for available options.

B<%constructor_arguments>
is a key–value list of explicit constructor arguments.

B<Returns>
a new Parser instance.

B<Overriding>:
This C<new_with()> method handles subclasses correctly:
It will create a new instance of the subclass, rather than of this Parser class.
However, it does not know about any state a subclass might have.
Therefore, all subclasses should override this method.
Simply specify the current state of your class and then append the user-specified C<%constructor_arguments>:

    package MyParser;
    use Moo;
    extends 'MarpaX::Grammar::Preprocessor::Parser';
    use namespace::clean;

    has _custom_state => (is => 'ro', required => 1);

    around new_with => sub {
        my ($orig, $self, %constructor_arguments) = @_;
        return $orig->(
            $self,
            _custom_state => $self->_custom_state,
            %constructor_arguments,
        ):
    };

=cut

sub new_with {
    my ($self, %override_args) = @_;

    my %args;
    while (my ($arg_name, $method) = each %CTOR_ARG_MAPPING) {
        $args{$arg_name} = $self->$method;
    }

    return $self->new(
        %args,
        %override_args,
    );
}

=head2 write

=head2 write_deferred

    $parser->write(@slif_fragments)
    $parser->write_deferred(@slif_fragments)

append the C<@slif_fragments> to the output buffer,

You should make sure that the SLIF fragment is valid
in the context of the current location.
If the SLIF fragment does not necessarily have to stand right here
but must merely occur at some point in the output,
then you can write to the deferred buffer with C<write_deferred()>.

Note that writing to a buffer is an uncomposable side effect.
If your command only produces a single value,
you should probably use it as a return value.
This way, other commands can apply additional transformations,
The preprocessor will eventually write any returned value to the buffer.

B<@slif_fragments>
is a list of strings that you want to write to the buffer.
As with the builtins C<say> and C<print>, the strings are joined with no space in between.

B<Example:>

    $parser->write("# frobnicate the next symbol\n");
    $parser->write_deferred("HelperRule ::= Thing; ");

=cut

sub write {
    my ($self, @things) = @_;
    my $buffer = $self->$_buffer;

    $$buffer .= $_ for @things;

    return;
}

sub write_deferred {
    my ($self, @things) = @_;
    my $buffer = $self->$_buffer_deferred;

    $$buffer .= $_ for @things;

    return;
}

=head2 pump

    ($type, $value) = $parser->pump(@terminating_types)

poll for tokens until a C<@terminating_types> token is found.
Token values are appended to the currently selected buffer.

This method is called by parser clients to drive the parser through the input.

B<@terminating_types>: list of L<TokenType|MarpaX::Grammar::Preprocessor::TokenType>

the tokens upon which you want to terminate pumping.

B<Returns> the C<($type, $value)> pair of the terminating token.
This token is not written to the buffer.

B<Throws> if C<next_token()> throws at any point.
Also throws when a terminator token was found that cannot be written to the buffer.
Such terminator tokens are C<EOF> and C<CLOSE>.
If you want to terminate pumping on such a token,
you must explicitly list it in the C<@terminating_types>.

B<Example>:

    my $parser = ...
    $parser->pump('EOF');
    my $result = $parser->result;

=cut

sub pump {
    my ($self, @terminating_types) = @_;
    if (not @terminating_types) {
        _::croak "pump() requires at least one terminating type";
    }
    my %terminating_types = map { $TOKEN_TYPE->coerce($_) => undef } @terminating_types;

    # pump the whole source through the preprocessor
    while (1) {
        my ($type, $value) = $self->next_token
            or _::croak "next_token() in pump() didn't return a token";
        return ($type, $value) if exists $terminating_types{$type};

        # if the token is not a requested terminator,
        # but can't be printed because it would usually be a terminator,
        # then throw an error.
        _::croakf q(Unexpected token %s(%s)), $type, _::pp $value
            if _::any { $type eq $_ } qw/EOF CLOSE/;

        $self->write($value);
    }
}

=head2 next_token

    ($type, $value) = $parser->next_token;

Retrieve the next token from the input, applying special commands as needed.

This method is the main parsing driver.
It implements various special syntax,
and delegates to C<command_*()> methods when a command is encountered.
Note that whitespace and commends
are directly written to the currently selected buffer
rather than being returned as a token.

B<Returns> a two-value list:
B<$type> is the L<TokenType|MarpaX::Grammar::Preprocessor::TokenType> of the found token.
B<$value> is the arbitrary value of the token,
which in general would be a string.
When the end of the input is encountered, it returns an C<EOF> token.

B<Throws> on illegal syntax.

B<Stability:>
The interface of this method is not expected to change,
though new behaviour might be added in a backwards-compatible manner.

B<Example:>

    while (1) {
        my ($type, $value) = $self->next_token
        last if $type == 'CLOSE' or $type == 'EOF';
        die "Expected IDENT" if not $type == 'IDENT';
        do_something_with($value);
    }

=cut

sub next_token {
    my ($self) = @_;

    my $NAMESPACE_SEPARATOR = $self->$_namespace_separator;

    my $source_ref = $self->$_source_ref;

    for ($$source_ref) {
        use re '/sx';

        if (not defined pos) {
            _::croak "input source match position was not defined";
        }

        TRY:
        while (pos() < length()) {
            # write white space and comments directly to the current buffer
            if (m/\G( \s+ | [#] [^\n]* )/gc) {
                $self->write($1);
                next TRY;
            }

            # read identifiers
            # FIXME this only accepts Perl-like identifiers,
            # but the SLIF allows rules to include spaces, like
            # <foo bar complex rule name>
            if (m/\G( \w+ )/gc) {
                return $TT_IDENT, $1;
            }

            # read namespace references and namespaced identifiers
            if (m/\G [%] /gc) {
                # uncoverable condition right
                my $ns = $self->$_namespace
                    // _::croak q(No namespace was set);

                if (m/\G( \w+ )/gc) {
                    return $TT_IDENT, "${ns}${NAMESPACE_SEPARATOR}${1}";
                }
                else {
                    return $TT_IDENT, $ns;
                }
            }

            # read string literals
            if (m/\G( ['] [^']+ ['] (?: [:][i] )? )/gc) {
                return $TT_LITERAL, $1;
            }

            # read charclass literals
            if (m/\G( [\[] (?: [^\]\\]++ | [\\]. )+ [\]] )/gc) {
                return $TT_LITERAL, $1;
            }

            # read command invocations
            if (m/\G [\\] (\w+)/gc) {
                my $command = $1;
                my $method_name = "command_${command}";
                my $handler = $self->can($method_name)
                    or _::croak "Unknown command $command, could not find $method_name method";
                my ($type, $value) = $handler->($self);
                return $TOKEN_TYPE->coerce($type), $value;
            }

            # read inline rule
            if (m/\G [{]/gc) {
                my $scoped = $self->new_with(
                    buffer => do { my $b = ''; \$b },
                    buffer_deferred => do { my $b = '; '; \$b },
                );
                # the first identifier in the scope is the name of the inline rule
                my $name = $scoped->expect($TT_IDENT);
                $scoped->write($name);

                # pump the whole inline rule into the deferred buffer
                # Throw if the inline rule wasn't terminated via a CLOSE brace.
                my ($type, $value) = $scoped->pump($TT_EOF, $TT_CLOSE);
                if ($type != $TT_CLOSE) {
                    _::croak "Expecting closing brace for inline rule, but got $type";
                }

                $self->write_deferred($scoped->result->slif_source);

                # return the name of the inline rule
                return $TT_IDENT, $name;
            }

            # read closing token
            if (m/\G [}]/gc) {
                return $TT_CLOSE, undef;
            }

            # read docstring
            if (m/\G (?= ["]["]["] )/gc) {
                return $self->command_doc;
            }

            # read anything else as an OP
            if (m/\G( [^\s\#\w\%\'\[\\]+ )/gc) {
                return $TT_OP, $1
            }

            my $source_around_here = substr $_, pos, 20;
            $source_around_here =~ s/...\z/.../s if length() - pos() > 20;
            die sprintf "Illegal code path taken: unknown grammar string at %s",
                _::pp $source_around_here;
        }

        return $TT_EOF, undef;
    }
}

=head2 expect

    $value = $parser->expect(@expected_types);

fetch the value of the C<next_token()> if it matches any of the C<@expected_types>.

This function is useful for straight-forward token-based custom parsing of the source.
You tell it what types of tokens you expect,
and either get the value of a matching token,
or an exception.
If you want to switch on the token type, use C<next_token()> directly without this wrapper.
If you do not want other commands to get a chance at processing the next token before you get it,
use regular expressions instead.

B<@expected_types> is a list of expected L<TokenType|MarpaX::Grammar::Preprocessor::TokenType>s.
Any one of these must match.

B<Returns> the value of the next token, if it matched any of the C<@expected_types>.

B<Throws>
when the next token does not match any C<@expected_types>,
or when the end of the input was reached,
or when C<next_token()> throws here.

B<Example:>

    sub command_sep {
        my ($self) = @_;
        my $sep = $self->expect('IDENT', 'LITERAL');
        return OP => "separator => $sep";
    }

=cut

sub expect {
    my ($self, @expected) = @_;
    my ($type, $value) = $self->next_token;
    return $value if _::any { $type == $_ } @expected;
    _::croakf "expected {%s} but found %s",
        (join ', ', @expected), $type;
}

=head1 COMMANDS

This section contains reference documentation for all preprocessor commands.
For a general overview of the available sytax,
see the L<main module description|MarpaX::Grammar::Preprocessor/"DESCRIPTION"> instead.

=head2 command \array

=head2 command \null

=head2 command \do

=for Pod::Coverage
    command_array
    command_null
    command_do

    \array
    \null
    \do ActionName

Specify semantics for a rule.

The C<\do> command expects an C<ActionName> which must be a legal Perl bareword.
It expects that C<do_ActionName> exists
as a subroutine in the chosen semantics package
or as a method in the provided action object.

B<Expands to> an "action" adverb with the specified semantics:

    action => ::array  # \array
    action => ::undef  # \null
    action => do_ActionName  # \do ActionName

B<Example:>

    List ::= Item* \array
    IgnoreMy ::= Value \null
    Assignment ::= Var ('=') Expression \do Assignment

=cut

sub command_array { return $TT_OP, 'action => ::array' }
sub command_null  { return $TT_OP, 'action => ::undef' }
sub command_do {
    m/\G \s+ (\w+)/xgc
        or die "Expected action name";
    return $TT_OP, "action => do_${1}";
}

=head2 command \group

=head2 command \left

=head2 command \right

=for Pod::Coverage
    command_group
    command_left
    command_right

    \group
    \left
    \right

B<Expands to> an "assoc" adverb with the specified associativity:

    assoc => group  # \group
    assoc => left   # \left
    assoc => right  # \right

B<Example:>

    Expression
        ::= Literal
        ||  ('(') Expression (')') \group
        ||  Expression ('^') Expression \right
        ||  Expression ('+') Expression \left

=cut

sub command_group { return $TT_OP, 'assoc => group' }
sub command_left  { return $TT_OP, 'assoc => left'  }
sub command_right { return $TT_OP, 'assoc => right' }

=head2 command \lax

=for Pod::Coverage
    command_lax

    \lax

In a sequence rule, allow a trailing separator.
E.g. in Perl lists may end with a superfluous comma: C<[1, 2, 3,]>
whereas JSON requires proper separation.

B<Expands to:>

    proper => 0

B<Example:>

    List ::= Item* \lax \sep COMMA \array

=cut

sub command_lax { return $TT_OP, 'proper => 0' }

=head2 command \sep

=for Pod::Coverage
    command_sep

    \sep RULE

Specify a separator for a sequence rule

B<RULE> is an C<IDENT> or C<LITERAL> token.

B<Expands to:>

    separator => RULE

B<Example:>

    List ::= Item* \sep COMMA

=cut

sub command_sep {
    my ($self) = @_;
    my $sep = $self->expect($TT_IDENT, $TT_LITERAL);
    return $TT_OP, "separator => ${sep}";
}

=head2 command \keyword

=for Pod::Coverage
    command_keyword

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
    my $name = $self->expect($TT_IDENT);
    $self->write(":lexeme ~ $name priority => 1;");
    return $TT_IDENT, $name;
}

=head2 command \namespace

=for Pod::Coverage
    command_namespace

    \namespace IDENT

Set the current namespace context,
so that it can be used for C<%> namespace references
or C<%name> internal rules.

B<IDENT>
is an C<IDENT> token.

B<Expands to> the C<IDENT> token,

B<Example:>

    \namespace Expression
        ::= % ('+') % \do Addition
        ||  ...

    \namespace List
        ::= ('[') %Items (']')
        %Items ::= Item* \sep COMMA

=cut

sub command_namespace {
    my ($self) = @_;
    my $name = $self->expect($TT_IDENT);
    $self->$_namespace($name);
    return $TT_IDENT, $name;
}

=head2 command \doc

=for Pod::Coverage
    command_doc

    \doc hide IDENT
    \doc DOCSTRING IDENT
    DOCSTRING IDENT

Associate a C<DOCSTRING> with a given C<IDENT>,
or C<hide> a symbol from the doc system.

You can retrieve the documentation hash via L<C<docs()>|MarpaX::Grammar::Preprocessor::Result/"docs"> from the result object.

The C<\doc> command can be omitted when using docstrings:
C<\doc """ foo> and C<""" foo> are equivalent.
However, you must explicitly use the command when using it to hide a symbol from the documentation system:
C<\doc hide>.

B<IDENT>
is the symbol you want to document.

B<DOCSTRING> is at least one docstring line,
Each line starts with a triple quote and a space,
and then runs until the end of the line.

B<Expands to> the C<IDENT> token.

B<Example:>

    \doc hide
    InternalSymbol ::= ...

    """ a sequence of zero or more values
    List ::= Value* \sep COMMA \array

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
        die sprintf "Expected docstring near %s...", _::pp $next_source;
    }

    my $symbol = $self->expect($TT_IDENT);
    die "Symbol $symbol already documented" if exists $self->$_docs->{$symbol};
    $self->$_docs->{$symbol} = $docs;
    return $TT_IDENT, $symbol;
}

=head2 command \optional

=for Pod::Coverage
    command_optional

    \optional RULE

Make a G1 C<RULE> optional.

The optional version of this rule will have a null case that carries an undef value,
and a case that proxies to the given C<RULE>.

This is really comfortable if you have an optional right hand side item in a larger rule.
Otherwise, you would have to create an explicit optional rule (like this command does)
or create two alternatives of the right hand side where one alternative does not include the optional rule.

B<RULE>:
an C<IDENT> token of a G1 symbol, i.e. a rule that was declared like C<RULE ::= ...>.
The C<RULE> should not be nullable itself.

B<Expands to> a gensym'd name referring to an optional version of the rule.

B<Inserts:>

    RULE__Optional ::= action => ::undef;
    RULE__Optional ::= RULE action => ::first

B<Example:>

    \namespace
    Conditional ::=
        (KW_IF) Expression
        COMPOUND_STATEMENT
        \optional { %Else ::= (KW_ELSE) COMPOUND_STATEMENT }
        \do Conditional

=cut

sub command_optional {
    my ($self) = @_;
    my $cache_ref = $self->$_optional_rule_cache;
    my $rule = $self->expect($TT_IDENT);
    # uncoverable condition false
    my $optional_rule = $cache_ref->{$rule} //= do {
        my $optional_rule = $rule . $self->$_namespace_separator . "Optional";

        $self->write_deferred("$optional_rule ::= action => ::undef; $optional_rule ::= $rule action => ::first; ");

        $optional_rule;
    };
    return $TT_IDENT, $optional_rule;
}

1;
