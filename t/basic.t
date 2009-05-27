use strict;
use warnings;
use Test::More tests => 28*2;
use Test::Exception;

{ package Trait;
  use Moose::Role;
  has 'foo' => (
      is       => 'ro',
      isa      => 'Str',
      required => 1,
  );

  package Class;
  use Moose;
  with 'MooseX::Traits';

  package Another::Trait;
  use Moose::Role;
  has 'bar' => (
      is       => 'ro',
      isa      => 'Str',
      required => 1,
  );

  package Another::Class;
  use Moose;
  with 'MooseX::Traits';
  has '+_trait_namespace' => ( default => 'Another' );

  package NS1;
  use Moose;

  package NS1::Trait::Foo;
  use Moose::Role;
  has 'bar' => (is => 'ro', required => 1);

  package NS2;
  use Moose;
  use base 'NS1';
  with 'MooseX::Traits';
  has '+_trait_namespace' => ( default => '+Trait' );

  package NS2::Trait::Bar;
  use Moose::Role;
  has 'baz' => (is => 'ro', required => 1);
}

my @method = (
    sub {
        my $class = shift;
        return $class->new_with_traits(@_)
    },
    sub {
        my $class = shift;
        my %args = @_;
        my @traits = @{ delete $args{traits} || [] };

        # any nicer way to do this?
        my $self = $class->new(%args);
        while (my ($k, $v) = each %args) {
            $self->{$k} = $v unless exists $self->{$k};
        }

        $self->apply_traits(@traits);
        return $self
    },
);

for my $new_with_traits (@method) {
{
    my $instance = Class->$new_with_traits( traits => ['Trait'], foo => 'hello' );
    isa_ok $instance, 'Class';
    can_ok $instance, 'foo';
    is $instance->foo, 'hello';
    isnt ref($instance), 'Class';
    is $instance->_original_class_name, 'Class';
}

{
# Carp chokes here
    no warnings 'redefine';
    local *Carp::caller_info = sub {};

    throws_ok {
        Class->$new_with_traits( traits => ['Trait'] );
    } qr/required/, 'foo is required';
}

{
    my $instance = Class->$new_with_traits;
    isa_ok $instance, 'Class';
    ok !$instance->can('foo'), 'this one cannot foo';
}
{
    my $instance = Class->$new_with_traits( traits => [] );
    isa_ok $instance, 'Class';
    ok !$instance->can('foo'), 'this one cannot foo either';
}
{
    my $instance = Another::Class->$new_with_traits( traits => ['Trait'], bar => 'bar' );
    isa_ok $instance, 'Another::Class';
    can_ok $instance, 'bar';
    is $instance->bar, 'bar';
}
{
    my $instance = Another::Class->$new_with_traits(
        traits   => ['Trait', '+Trait'],
        foo      => 'foo',
        bar      => 'bar',
    );
    isa_ok $instance, 'Another::Class';
    can_ok $instance, 'foo';
    can_ok $instance, 'bar';
    is $instance->foo, 'foo';
    is $instance->bar, 'bar';
}
{
    my $instance = NS2->$new_with_traits(
        traits   => ['+Trait', 'Foo', 'Bar'],
        foo      => 'foo',
        bar      => 'bar',
        baz      => 'baz',
    );
    isa_ok $instance, 'NS2';
    isa_ok $instance, 'NS1';
    ok $instance->meta->does_role('NS1::Trait::Foo');
    ok $instance->meta->does_role('NS2::Trait::Bar');
    can_ok $instance, 'foo';
    can_ok $instance, 'bar';
    can_ok $instance, 'baz';
    is $instance->foo, 'foo';
    is $instance->bar, 'bar';
    is $instance->baz, 'baz';
}
}
