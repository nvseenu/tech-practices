import unittest
import algo_ds.heap as heap
from algo_ds.heap import MaxHeap, MinHeap


class TestHeap(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_max_heapify(self):
        heaps = [16, 4, 10, 14, 7, 9, 3, 2, 8, 1]
        heap.max_heapify(heaps, 1)
        heaps1 = [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]
        self.assertEqual(heaps1, heaps)

    def test_build_max_heap(self):
        expected_heaps = [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]
        heaps = [4, 1, 3, 2, 16, 9, 10, 14, 8, 7]
        heap.build_max_heap(heaps)
        self.assertEqual(expected_heaps, heaps)

    def test_max_heap(self):
        arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

        h = MaxHeap()
        for i in arr:
            h.insert(i, i)

        expected_heaps = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
        for i in expected_heaps:
            self.assertEqual(i, h.extract_max(), f"For i:{i}, Got unexpected maximum value")

    def test_increase_key(self):
        arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

        h = MaxHeap()
        for i in arr:
            h.insert(i, i)

        # Increase key 1 to 20
        h.increase_key(1, 20)

        expected_heaps = [1, 10, 9, 8, 7, 6, 5, 4, 3, 2]
        for i in expected_heaps:
            self.assertEqual(i, h.extract_max(), f"Got unexpected maximum value")

    def test_max_heap_for_custom_object(self):

        jobs = [Job(i, "Job" + str(i)) for i in range(1, 11)]

        h = MaxHeap()
        for job in jobs:
            h.insert(job, job._id)

        expected_jobs = [jobs[i] for i in range(9, -1, -1)]
        for job in expected_jobs:
            self.assertEqual(job, h.extract_max(), f"Got unexpected maximum value")

    def test_min_heap(self):
        arr = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

        h = MinHeap()
        for i in arr:
            h.insert(i, i)

        expected_heaps = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        for i in expected_heaps:
            self.assertEqual(i, h.extract_min(), f"For i:{i}, Got unexpected minimum value")

    def test_get_for_min_heap(self):
        arr = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

        h = MinHeap()
        for i in arr:
            h.insert(i, i)

        self.assertEqual(10, h.get(10), "Got an unexpected value")
        self.assertEqual(5, h.get(5), "Got an unexpected value")

    def test_increase_key_with_min_heap(self):
        arr = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

        h = MinHeap()
        for i in arr:
            h.insert(i, i)

        # Increase key 1 to 20m now top item will be 2, as 1 has been moved
        # to last position
        h.increase_key(1, 20)

        expected_heaps = [2, 3, 4, 5, 6, 7, 8, 9, 10, 1]
        for i in expected_heaps:
            self.assertEqual(i, h.extract_min(), f"Got unexpected minimum value")

    def test_min_heap_for_custom_object(self):

        jobs = [
            Job(4, "Job4"),
            Job(3, "Job3"),
            Job(2, "Job2"),
            Job(1, "Job1")
        ]

        h = MinHeap()
        for job in jobs:
            h.insert(job, job._id)

        expected_jobs = [
            Job(1, "Job1"),
            Job(2, "Job2"),
            Job(3, "Job3"),
            Job(4, "Job4")
        ]

        for job in expected_jobs:
            self.assertEqual(job, h.extract_min(), f"Got unexpected minimum value")


class Job:
    def __init__(self, id, name):
        self._id = id
        self._name = name

    def priority(self):
        return self._id

    def __eq__(self, other):
        return (self._id, self._name) == (other._id, other._name)

    def __str__(self):
        return f"Job[id={self._id}, name={self._name}"

    def __repr__(self):
        return str(self)
