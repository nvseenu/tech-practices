import os
import sys

class SinglyLinkedListNode:
    def __init__(self, node_data):
        self.data = node_data
        self.next = None

class SinglyLinkedList:
    def __init__(self):
        self.head = None
        self.tail = None

    def insert_node(self, node_data):
        node = SinglyLinkedListNode(node_data)

        if not self.head:
            self.head = node
        else:
            self.tail.next = node


        self.tail = node

def print_singly_linked_list(node, sep, fptr):
    while node:
        fptr.write(str(node.data))

        node = node.next

        if node:
            fptr.write(sep)



def compare_lists(list1, list2):
    while True:
        if list1 == None and list2 == None:
            return 1
        
        if list1 == None or list2 == None:
            return 0 

        if list1.data  != list2.data:
            return 0

        list1 = list1.next    
        list2 = list2.next

def merge_lists(head1, head2):
    if head1 == None:
        return head2

    res = SinglyLinkedList()    

    h1 = head1
    h2 = head2
    while h1 and h2:
        if h1.data < h2.data:
            res.insert_node(h1.data)
            h1 = h1.next
        else:
            res.insert_node(h2.data)    
            h2 = h2.next

    while h1:
        res.insert_node(h1.data)
        h1 = h1.next

    while h2:
        res.insert_node(h2.data)
        h2 = h2.next    

    return res.head    

def items(head):
    a = []
    while head:
        a.append(head.data)
        head = head.next

    return a    





