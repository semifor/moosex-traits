package MooseX::Traits;
use Moose::Role;
use Moose::Util ();
use Scalar::Util ();

our $VERSION   = '0.03';
our $AUTHORITY = 'id:JROCKWAY';

has '_trait_namespace' => (
    # no accessors or init_arg
    init_arg => undef,
    isa      => 'Str',
);

# shamelessly stolen from MX::Object::Pluggable
has _original_class_name => (
  is => 'ro',
  required => 1,
  isa => 'Str',
  default => sub{ Scalar::Util::blessed($_[0]) },
);

# dont pollute the consuming class with methods they don't want
my $find_trait = sub {
    my ($class, $base, $name) = @_;

    my @search_ns = grep !/^(?:Moose|Class::MOP)::/,
        $class->meta->class_precedence_list;

    for my $ns (@search_ns) {
        my $full = "${ns}::${base}::${name}";
        return $full if eval { Class::MOP::load_class($full) };
    }
};

my $transform_trait = sub {
    my ($class, $name) = @_;
    my $namespace = $class->meta->get_attribute('_trait_namespace');
    my $base;
    if($namespace->has_default){
        $base = $namespace->default;
        if(ref $base eq 'CODE'){
            $base = $base->();
        }
    }

    return $name unless $base;
    return $1 if $name =~ /^[+](.+)$/;
    return $class->$find_trait($1, $name) if $base =~ /^\+(.*)/;
    return join '::', $base, $name;
};

my $resolve_traits = sub {
    my $class = shift;
    Class::MOP::load_class($_)
        for map { $_ = $class->$transform_trait($_) } @_;
};

sub new_with_traits {
    my ($class, %args) = @_;

    my $original_class = $class;

    if (my $traits = delete $args{traits}) {
        if(@$traits){
            $class->$resolve_traits(@$traits);

            my $meta = $class->meta->create_anon_class(
                superclasses => [ blessed($class) || $class ],
                roles        => $traits,
                cache        => 1,
            );

            $meta->add_method('meta' => sub { $meta });
            $class = $meta->name;
        }
    }

    my $constructor = $class->meta->constructor_name || 'new';

    return $class->$constructor(%args, _original_class_name => $original_class);
}

sub apply_traits {
    my ($self, @traits) = @_;

    if (@traits) {
        $self->$resolve_traits(@traits);

        Moose::Util::apply_all_roles($self, @traits);
    }
}

no Moose::Role;

1;

__END__

=head1 NAME

MooseX::Traits - automatically apply roles at object creation time

=head1 SYNOPSIS

Given some roles:

  package Role;
  use Moose::Role;
  has foo => ( is => 'ro', isa => 'Int' required => 1 );

And a class:

  package Class;
  use Moose;
  with 'MooseX::Traits';

Apply the roles to the class at C<new> time:

  my $class = Class->new_with_traits( traits => ['Role'], foo => 42 );

Then use your customized class:

  $class->isa('Class'); # true
  $class->does('Role'); # true
  $class->foo; # 42

To apply traits to an existing instance:

  $self->apply_traits(qw/Role1 Role2/);

=head1 DESCRIPTION

Often you want to create components that can be added to a class
arbitrarily.  This module makes it easy for the end user to use these
components.  Instead of requiring the user to create a named class
with the desired roles applied, or applying roles to the instance
one-by-one, he can just pass a C<traits> parameter to the class's
C<new_with_traits> constructor.  This role will then apply the roles
in one go, cache the resulting class (for efficiency), and return a
new instance.  Arguments meant to initialize the applied roles'
attributes can also be passed to the constructor.

Alternatively, traits can be applied to an instance with C<apply_traits>,
arguments for initializing attributes in consumed roles can be in C<%$self>
(useful for e.g. L<Catalyst> components.)

=head1 METHODS

=over 4

=item B<< $class->new_with_traits(%args, traits => \@traits) >>

=item B<< $instance->apply_traits(@traits) >>

=back

=head1 ATTRIBUTES YOUR CLASS GETS

This role will add the following attributes to the consuming class.

=head2 _trait_namespace

You can override the value of this attribute with C<default> to
automatically prepend a namespace to the supplied traits.  (This can
be overridden by prefixing the trait name with C<+>.)

Example:

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

  my $instance = Another::Class->new_with_traits(
      traits => ['Trait'], # "Another::Trait", not "Trait"
      bar    => 'bar',
  );
  $instance->does('Trait')          # false
  $instance->does('Another::Trait') # true

  my $instance2 = Another::Class->new_with_traits(
      traits => ['+Trait'], # "Trait", not "Another::Trait"
  );
  $instance2->does('Trait')          # true
  $instance2->does('Another::Trait') # false

If the value of L</_trait_namespace> starts with a C<+> the namespace will be
considered relative to the C<class_precedence_list> of the original class.

Example:

  package Class1
  use Moose;

  package Class1::Trait::Foo;
  use Moose::Role;
  has 'bar' => (
      is       => 'ro',
      isa      => 'Str',
      required => 1,
  );

  package Class2;
  use parent 'Class1';
  with 'MooseX::Traits';
  has '+_trait_namespace' => (default => '+Trait');

  package Class2::Trait::Bar;
  use Moose::Role;
  has 'baz' => (
      is       => 'ro',
      isa      => 'Str',
      required => 1,
  );

  package main;
  my $instance = Class2->new_with_traits(
      traits => ['Foo', 'Bar'],
      bar => 'baz',
      baz => 'quux',
  );

  $instance->does('Class1::Trait::Foo'); # true
  $instance->does('Class2::Trait::Bar'); # true

=head2 _original_class_name

When traits are applied to your class or instance, you get an anonymous class
back whose name will be not the same as your original class. So C<ref $self>
will not be C<Class>, but C<< $self->_original_class_name >> will be.

=head1 AUTHOR

Jonathan Rockway C<< <jrockway@cpan.org> >>

Stevan Little C<< <stevan.little@iinteractive.com> >>

Rafael Kitover C<< <rkitover@cpan.org> >>

=head1 COPYRIGHT AND LICENSE

Copyright 2008 Infinity Interactive, Inc.

L<http://www.iinteractive.com>

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

