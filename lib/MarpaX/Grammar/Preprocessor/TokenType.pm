use strict;
use warnings;
use utf8;

=pod

=encoding utf8

=head1 NAME

MarpaX::Grammar::Preprocessor::TokenType - token IDs for extended SLIF grammars

=cut

package MarpaX::Grammar::Preprocessor::TokenType;
use Moo;

use Util::Underscore v1.3.0;

my $REGISTRY_BY_NAME = {};

=head1 CONSTRUCTOR

    $token_type = MarpaX::Grammar::Preprocessor::TokenType->new($name);

add a new token type to the token type registry.

The constructor registers the token type object in a global registry,
from where it can be retrived via the C<coerce()> accessor.
It is not possible to un-register a token type.

B<$name>
is a string that names this token type,
and is suitable for human-readable error messages.
The name must be unique for all token types.
Ideally, the name is a legal Perl bareword.

B<Throws> when the name is already in use for a different token type.

=cut

has name => (is => 'ro', required => 1);

sub BUILDARGS {
    my ($class, $name) = @_;
    return { name => $name };
}

sub BUILD {
    my ($self) = @_;

    if ($REGISTRY_BY_NAME->{$self->name}) {
        _::croakf "cannot redefine %s", $self->name;
    }

    $REGISTRY_BY_NAME->{$self->name} = $self;
    return;
}

=head1 METHODS

=head2 name

    $name = $token_type->name;

B<Returns> the name as a string.

B<Throws> never.

=head2 operator ""

    "$token_type"

Convert to a string.

B<Returns> the L<C<name()>|/"name">.

=head2 operator ==
=head2 operator eq

    $token_a == $token_b
    $token_a eq $token_b

Compare two TokenTypes for equivalence.

B<$token_a>, B<$token_b> can either be TokenType instances
or a string that names a TokenType.
The input is passed to C<coerce()> before performing the comparison.

B<Returns> true when both operands refer to the same TokenType.

B<Throws> if an operand can't be coerced to a TokenType.

=cut

use overload (
    '""' => sub { shift->name },
    '==' => '_MarpaX_Grammar_Preprocessor_TokenType_operator_EQ',
    'eq' => '_MarpaX_Grammar_Preprocessor_TokenType_operator_EQ',
);

sub _MarpaX_Grammar_Preprocessor_TokenType_operator_EQ {
    my ($x, $y) = @_;
    return (_::ref_addr coerce(undef, $x)) == (_::ref_addr coerce(undef, $y));
}

=head2 coerce

    $token_type = MarpaX::Grammar::Preprocessor::TokenType->coerce($name);
    $token_type = MarpaX::Grammar::Preprocessor::TokenType->coerce($token_type);

Coerce a name to a token type, by looking it up in the global registry.

B<Returns> a token type if a token type with the given name exists.
Returns the given object if it already is a token type.

B<Throws> when the given name does not specify a token type.
You have to define token types via the L<constructor|/"CONSTRUCTOR">
before you can use them.

=cut

sub coerce {
    my ($class, $name_or_token_type) = @_;
    return $name_or_token_type
        if _::is_instance $name_or_token_type, __PACKAGE__;
    return $REGISTRY_BY_NAME->{$name_or_token_type}
        // _::croakf q(No token type for name=%s), $name_or_token_type;
}

=head2 all_token_types

    my @token_types = MarpaX::Grammar::Preprocessor::TokenType->all_token_types;

B<Returns> all known token types, in no particular order.

B<Throws> never.

=cut

sub all_token_types {
    return values %$REGISTRY_BY_NAME;
}

=head1 TOKEN TYPES

There are various predefined token types.
These can be accessed as named constants, e.g.

    my $ident_token_type = MarpaX::Grammar::Preprocessor::TokenType->IDENT;

=head2 IDENT

The TokenType for identifiers.

An identifier is any symbol that can occur on the left hand side of a SLIF rule definition.

=head2 LITERAL

The TokenType for literals.

A literal is any terminal on the right hand side of a rule,
e.g. strings C<'foo'> or character classes C<[\w-]>.

=head2 OP

The TokenType for non-rule parts of the SLIF source.

This token is used for SLIF operators, adverbs,
or anything else that can't be classified as a more specific TokenType.
It should therefore be treated as a catch-all token type,
rather than being explicitly C<expect()>ed.

=head2 CLOSE

The TokenType for closing scopes.

This token signals that an inline rule is coming to an end.

=cut

# initialize the pre-defined token types
__PACKAGE__->new('IDENT');
__PACKAGE__->new('LITERAL');
__PACKAGE__->new('OP');
__PACKAGE__->new('CLOSE');

1;
