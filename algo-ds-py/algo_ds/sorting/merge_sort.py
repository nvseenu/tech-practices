class MergeSort:

    def sort(self, arr):
        self._merge_sort(arr, 0, len(arr) - 1)

    def _merge_sort(self, arr, start, end):
        #print(f"merge_sort start: {start}, end: {end}")
        if start < end:
            mid = start + (end - start) // 2
            self._merge_sort(arr, start, mid)
            self._merge_sort(arr, mid + 1, end)
            self._merge(arr, start, mid, end)

    def _merge(self, arr, start, mid, end):
        #print(f"merge start: {start}, mid: {mid}, end: {end}")

        arr1 = arr[start:mid + 1]
        arr2 = arr[mid + 1: end + 1]
        i1 = 0
        i2 = 0
        i = start

        while i1 < len(arr1) and i2 < len(arr2):
            if arr1[i1] < arr2[i2]:
                arr[i] = arr1[i1]
                i1 += 1
            else:
                arr[i] = arr2[i2]
                i2 += 1
            i += 1

        # Copy remaining items if any
        while i1 < len(arr1):
            arr[i] = arr1[i1]
            i1 += 1
            i += 1

        # Copy remaining items if any
        while i2 < len(arr2):
            arr[i] = arr2[i2]
            i2 += 1
            i += 1

        #print(f"After merge : {arr[start: end+1]}")
