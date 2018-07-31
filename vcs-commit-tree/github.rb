require_relative "lib/git_commit_tree/reader"
require_relative "lib/git_commit_tree/core"


# reader = GitCommitTree::Reader::GithubCommitReader.new('nvseenu', 'tech-practices')
client = GitCommitTree::Reader::GithubClient.new("nvseenu", "C0ntr01@16", "nvseenu", "tech-practices")
reader = GitCommitTree::Reader::GithubCommitReader.new(client)
tbuilder = GitCommitTree::Core::TreeBuilder.new
tree = tbuilder.build(reader)
puts "tree : #{tree}"
puts "Found branches: #{tree.branches.length}"
tree.branches.each do |b|
   puts "branch = #{b}"
   commits = tree.commits(b)
   commits.each { |c| puts c}
end
