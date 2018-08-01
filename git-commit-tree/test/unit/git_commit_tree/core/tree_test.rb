require "minitest/autorun"
require_relative "../../../../lib/git_commit_tree/core"

class TestTree < MiniTest::Test
  def setup
    @master_commits = [
      GitCommitTree::Core::Commit.new(
        id: "mc1", 
        message: "commit",
        date: Time.now,
        branch: "master",
        author: "user"),
      GitCommitTree::Core::Commit.new(
        id: "mc2", 
        message: "commit",
        date: Time.now,
        branch: "master",
        author: "user"),
      GitCommitTree::Core::Commit.new(
        id: "mc3", 
        message: "commit",
        date: Time.now,
        branch: "master",
        author: "user"),
      GitCommitTree::Core::Commit.new(
        id: "mc4", 
        message: "commit",
        date: Time.now,
        branch: "master",
        author: "user"),
      GitCommitTree::Core::Commit.new(
        id: "mc5", 
        message: "commit",
        date: Time.now,
        branch: "master",
        author: "user"),
    ]

    @development_commits = [
      GitCommitTree::Core::Commit.new(
        id: "mc1", 
        message: "commit",
        date: Time.now,
        branch: "development",
        author: "user"),
      GitCommitTree::Core::Commit.new(
        id: "mc2", 
        message: "commit",
        date: Time.now,
        branch: "development",
        author: "user"),
      GitCommitTree::Core::Commit.new(
        id: "dc1", 
        message: "commit",
        date: Time.now,
        branch: "development",
        author: "user"),
      GitCommitTree::Core::Commit.new(
        id: "dc2", 
        message: "commit",
        date: Time.now,
        branch: "development",
        author: "user"),
    ]


    @stage_commits =  [
      GitCommitTree::Core::Commit.new(
        id: "mc1", 
        message: "commit",
        date: Time.now,
        branch: "stage",
        author: "user"),
      GitCommitTree::Core::Commit.new(
        id: "mc2", 
        message: "commit",
        date: Time.now,
        branch: "stage",
        author: "user"),
      GitCommitTree::Core::Commit.new(
        id: "sc1", 
        message: "commit",
        date: Time.now,
        branch: "stage",
        author: "stage"),
      GitCommitTree::Core::Commit.new(
        id: "sc2", 
        message: "commit",
        date: Time.now,
        branch: "stage",
        author: "stage"),
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
    assert_commits(@master_commits, commits)
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
    @development_commits.each {|c| tree.add_commit c}

    # Get all commits from the tree instance
    commits = tree.commits(:master).to_a
    assert_commits(@master_commits, commits)

    commits = tree.commits(:development).to_a
    assert_commits(@development_commits, commits)
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
    @master_commits.each {|c| tree.add_commit c }
    @development_commits.each {|c| tree.add_commit c }
    @stage_commits.each {|c| tree.add_commit c }    

    # Get all commits from the tree instance
    commits = tree.commits(:master).to_a
    assert_commits(@master_commits, commits)

    commits = tree.commits(:development).to_a    
    assert_commits(@development_commits, commits)

    commits = tree.commits(:stage).to_a
    assert_commits(@stage_commits, commits)
  end

  def assert_commits(commits1, commits2)
    assert_equal(commits1.length, commits2.length, "Length of the commits are not matching")

    commits1.each_with_index do |c, i|
      assert_equal(c, commits2[i], "Commits are not matching")
    end
  end

  def teardown; end
end
