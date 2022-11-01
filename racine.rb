#! /usr/bin/ruby
# frozen_string_literal: true

# Class to calculate the square root of positive integer.
class Racine
  def mean(sup, inf)
    (sup + inf) / 2
  end

  def sqrt_floor(num)
    return num if num < 2

    sup = num
    inf = 1
    while inf != sup - 1
      med = mean sup, inf
      if med * med > num
        sup = med
      else
        inf = med
      end
    end
    inf
  end

  def newton(u, v)
    k1 = 1 + v / u
    loop do
      k2 = k1
      k1 = (k1 * k1 + v) / (2 * k1 + u)
      return k1 if k1 == k2 || (k1.zero? && k2 == 1)
    end
  end

  def iterations(prec)
    if (prec % @base).zero?
      prec / @base
    else
      1 + prec / @base
    end
  end

  def rac
    k0 = sqrt_floor @number
    return [k0] if @iterations.zero? || k0 * k0 == @number

    u = 2 * @base_racine * k0
    v = @carre_base * (@number - k0 * k0)
    r = [k0]
    loop do
      k = newton u, v
      r.push k
      @iterations -= 1
      return r if @iterations.zero?

      v = @carre_base * (v - (u + k) * k)
      u = @base_racine * (u + 2 * k)
    end
  end

  def initialize(number, precision = 30, base = 10)
    if number.negative?
      print "Racine des nombres négatifs non implémentée\n"
      Racine.usage
    elsif precision.negative?
      print "Une precision négative n'a aucune signification\n"
      Racine.usage
    elsif base <= 0
      print "La granularité doit être stritement positive\n"
      Racine.usage
    else
      @number = number
      @base = base
      @iterations = iterations precision
      @base_racine = 10**@base
      @carre_base = @base_racine**2
    end
  end

  def racine
    floor, *fracs = rac
    e = floor.to_s
    return e if fracs.empty?

    ls = fracs.collect { |f| format("%0#{@base}d", f) }
    ([e, '.'] + ls).join('')
  end

  def self.usage
    print <<ENDOFUSAGE
    usage : racine <nombre> [ <précision> [ <granularité> ]]
    où <nombre>, <précision> et <granularité> sont des entiers positifs
ENDOFUSAGE
    exit 1
  end
end

if $PROGRAM_NAME == __FILE__
  Racine.usage unless ARGV.size.between?(1, 3)
  print Racine.new(*ARGV.collect(&:to_i)).racine, "\n"
end
# vim: syntax=ruby:
