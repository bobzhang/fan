#!/usr/bin/env ruby
file = File.new('cold.stat')

added = []
removed = []

while (line=file.gets)
  if line =~ /files changed/
    added.push(line[/(\d+) insertions\(\+\)/,1].to_i) # default zero
    removed.push(line [/(\d+) deletions\(\-\)/,1].to_i)
  end
end
added.reverse
