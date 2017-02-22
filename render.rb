require 'json'
require 'erb'

bla = JSON.parse(File.read "dummy.json")

anims = bla["anims"]
rotations = bla["rotations"]
nums = bla["num"]

File.write("new-dummy.mdl", ERB.new(File.read "template.erb").result())

