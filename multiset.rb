#!/usr/bin/env ruby

$bag = {}; $rws = []

def multiset(str)
  o = str.split(/\s+/).group_by{|i|i}.transform_values(&:length)
  if o.keys.all?(&:empty?) then {} else o end
end

def rewrite()
  loop do
    didMatch = false
    $rws.each do |pat, set|
      match = true
      pat.each do |term, count|
        (match = false; break) unless $bag.key? term
        (match = false; break) if $bag[term] < count
      end
      if match
        didMatch = true
        $bag.merge!(pat) { |_, bagged, removed| bagged - removed }
        $bag.merge!(set) { |_, bagged, newly| newly + bagged }
        puts $bag
        break
      end
    end 
    break unless didMatch
  end
end

while gets.chomp
  if $_ =~ /\s*(.*)\s*\|\s*(.*)\s*/
    $rws << [multiset($1), multiset($2)]
  else 
    $bag.merge!(multiset $_) { |_, bagged, newly| newly + bagged }
  end
  rewrite
end