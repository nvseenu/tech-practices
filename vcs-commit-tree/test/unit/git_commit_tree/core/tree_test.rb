require "minitest/autorun"
require_relative "../../../../lib/git_commit_tree/core"

class TestTree < MiniTest::Test
  def setup
    @master_commits = [
      GitCommitTree::Core::Commit.new("mc1", "commit", Time.now, :master, "user"),
      GitCommitTree::Core::Commit.new("mc2", "commit", Time.now, :master, "user"),
      GitCommitTree::Core::Commit.new("mc3", "commit", Time.now, :master, "user"),
      GitCommitTree::Core::Commit.new("mc4", "commit", Time.now, :master, "user"),
      GitCommitTree::Core::Commit.new("mc5", "commit", Time.now, :master, "user")
    ]

    @development_commits = [
      GitCommitTree::Core::Commit.new("dc1", "commit", Time.now, :development, "user"),
      GitCommitTree::Core::Commit.new("dc2", "commit", Time.now, :development, "user")
    ]

    @stage_commits = [
      GitCommitTree::Core::Commit.new("sc1", "commit", Time.now, :stage, "user"),
      GitCommitTree::Core::Commit.new("sc2", "commit", Time.now, :stage, "user")
    ]
  end

  #   Test if add_commit inserts given commit in below order
  #
  #     mc1 <- mc2 <- mc3
  #
  def test_add_commit
    # Insert all commits
    tree = GitCommitTree::Core::Tree.new
    @master_commits.each {|c| tree.add_commit c }

    # Get all commits from the tree instance
    commits = tree.commits(:master).to_a
    assert_commits(@master_commits.reverse, commits)
  end

  #
  #   Test if add_commit inserts given commit in below order  #
  #
  #    mc1 <- mc2 <- mc3 <- mc4 <- mc5
  #                   \
  #                    dc1 <- dc2
  #
  def test_add_commit_for_child_branch
    tree = GitCommitTree::Core::Tree.new
    @master_commits.each {|c| tree.add_commit c }
    @development_commits.each {|c| tree.add_commit(c, :master) }

    # Get all commits from the tree instance
    commits = tree.commits(:master).to_a
    assert_commits(@master_commits.reverse, commits)

    commits = tree.commits(:development).to_a
    exp_commits = @master_commits + @development_commits
    assert_commits(exp_commits.reverse, commits)
  end

  #
  #   Test if add_commit inserts given commit in below order
  #
  #                    sc1 -> sc2
  #                   /
  #    mc1 <- mc2 <- mc3 <- mc4 <- mc5
  #                   \
  #                    dc1 <- dc2
  #
  #
  def test_add_commit_for_multiple_branches
    tree = GitCommitTree::Core::Tree.new
    @master_commits[0, 3].each {|c| tree.add_commit c }
    @development_commits.each {|c| tree.add_commit(c, :master) }
    @stage_commits.each {|c| tree.add_commit(c, :master) }
    @master_commits[3, 2].each {|c| tree.add_commit c }

    # Get all commits from the tree instance
    commits = tree.commits(:master).to_a
    assert_commits(@master_commits.reverse, commits)

    commits = tree.commits(:development).to_a
    exp_commits = @master_commits[0, 3] + @development_commits
    assert_commits(exp_commits.reverse, commits)

    commits = tree.commits(:stage).to_a
    exp_commits = @master_commits[0, 3] + @stage_commits
    assert_commits(exp_commits.reverse, commits)
  end

  #
  #   Test if add_commit inserts given commit in below order
  #
  #
  #    mc1 <- mc2 <- mc3 <- mc4 <- mc5
  #             \
  #              dc1 <- dc2
  #
  #
  def test_add_commit_at_specific_commit_id
    tree = GitCommitTree::Core::Tree.new
    @master_commits.each {|c| tree.add_commit c }
    mc2 = @master_commits[2]
    @development_commits.each {|c| tree.add_commit(c, :master, mc2.id) }

    commits = tree.commits(:development).to_a
    exp_commits = @master_commits[0, 2] + @development_commits
    assert_commits(exp_commits.reverse, commits)
  end

  def assert_commits(commits1, commits2)
    assert_equal(commits1.length, commits2.length, "Length of the commits are not matching")

    commits1.each_with_index do |c, i|
      assert_equal(c, commits2[i], "Commits are not matching")
    end
  end

  def teardown; end
end
