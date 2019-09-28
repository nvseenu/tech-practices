def maximum_subarray_by_divide_and_conquer(arr):
    """
    Find a subarray which is containing maximum sum among other subarrays
    """
    return _max_subarray(arr, 0, len(arr) - 1)


def _max_subarray(arr, start, end):
    if start == end:
        return (arr[start], start, end)

    mid = start + (end - start) // 2

    sum1, start1, end1 = _max_subarray(arr, start, mid)
    sum2, start2, end2 = _max_subarray(arr, mid + 1, end)
    sum3, start3, end3 = _max_cross_subarray(arr, start, mid, end)

    max_sum = max(sum1, sum2, sum3)
    if sum1 == max_sum:
        return (sum1, start1, end1)
    elif sum2 == max_sum:
        return (sum2, start2, end2)
    else:
        return (sum3, start3, end3)


def _max_cross_subarray(arr, start, mid, end):
    sum = 0
    left_sum = None
    left_index = 0
    for i in range(mid, start - 1, -1):
        sum += arr[i]

        if not left_sum:
            left_sum = sum
            left_index = i
        elif sum > left_sum:
            left_sum = sum
            left_index = i

    sum = 0
    right_sum = None
    right_index = 0

    for i in range(mid + 1, end + 1, 1):
        sum += arr[i]
        if not right_sum:
            right_sum = sum
            right_index = i
        elif sum > right_sum:
            right_sum = sum
            right_index = i

    return (left_sum + right_sum, left_index, right_index)


def maximum_subarray_by_kadane_algorithm(arr):

    max_so_far = 0
    max_end_here = 0
    end = 0
    prev_end = 0

    for i in range(len(arr)):
        max_end_here += arr[i]

        if max_so_far <= max_end_here:
            max_so_far = max_end_here
            prev_end = end
            end = i

        if max_end_here < 0:
            max_end_here = 0

    return (max_so_far, prev_end, end)


def counting_valleys(s):
    valley = 0
    mountain = 0
    prev_step = 0
    step = 0
    for c in s:
        prev_step = step
        if c == 'D':
            step -= 1
        else:
            step += 1

        if step == 0 and prev_step < 0:
            valley += 1
        elif step == 0 and prev_step > 0:
            mountain += 1

    return valley


def count_matching_pair_of_socks(socks):
    """"
    John works at a clothing store. He has a large pile of socks that he must pair by color for sale.
    Given an array of integers representing the color of each sock,
    determine how many pairs of socks with matching colors there are.

    For example, Given array [1,2,1,2,1,3,2]  The number of pairs is 2.
    Because socks 1 and 2 has its pair, others dont.
    """
    socks_dict = {}
    for sock in socks:
        if sock in socks_dict:
            socks_dict[sock] = socks_dict[sock] + 1
        else:
            socks_dict[sock] = 1

    sum = 0
    for k, v in socks_dict.items():
        sum += (v // 2)

    return sum


def minimum_jumps_among_clouds(arr):
    i = 0
    jump_count = 0
    while i < len(arr) - 1:
        i = _find_next_jump(i, arr)
        if i != -1:
            jump_count += 1

    return jump_count


def _find_next_jump(i, arr, max_jumps=2):
    if arr[i + 1] == 1:
        return i + 2

    j = i
    while j < i + max_jumps and j < len(arr) - 1:

        if arr[j + 1] == 1:
            return j

        j += 1

    return j
