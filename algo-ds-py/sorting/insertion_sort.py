class InsertionSort:


    def sort(self, arr):
        """
        Sorts given array using insertion sort logic.
        Rate of growth is O(n^2)    
        """

        for i in range(1, len(arr)):
            key = arr[i]

            j = i - 1
            while j >= 0 and arr[j] > key:        
                arr[j+1] = arr[j]
                j = j-1

            arr[j+1] = key



