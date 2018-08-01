require "minitest/autorun"
require_relative "../../../../lib/git_commit_tree/core"

class TestGithubCommitReader < Minitest::Test
  def test_filter_branch_refs
    refs = [
      {"ref" => "refs/heads/master", :object => {"sha" => "123456"}},
      {"ref" => "refs/heads/branch1", :object => {"sha" => "32123"}},
      {"ref" => "refs/pull/master", :object => {"sha" => "54322"}}
    ]
  end
end
