require 'json'

module GitCommitTree
  module Core
    class Commit
      include Enumerable
      attr_reader :id, :message, :date, :branch, :author, :parents

      def initialize(id, message, date, branch, author, parents)
        raise "Arg id is nil" if id.nil?
        raise "Arg message is nil" if message.nil?
        raise "Arg date is nil" if date.nil?
        raise "Arg branch is nil" if branch.nil? || branch.empty?
        raise "Arg author is nil" if author.nil? || author.empty?

        @id = id
        @message = message
        @date = date
        @branch = branch
        @author = author
        @parents = parents
      end

      def initial_commit?
         @parents.empty?
      end

      def <=>(other)
        @id <=> other.id
      end

      def to_s
        "Commit { id: #{@id}, message: #{@message}, date: #{@date}, branch: #{@branch}, author: #{@author}, parents: #{@parents}"
      end

      def to_h
        {id: @id,
         message: @message}         
      end

      def to_json
        JSON.dump(to_h)  
      end
    end
  end
end
