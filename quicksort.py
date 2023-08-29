def quicksort(arr):
    if not arr:
        return []
    return quicksort([x for x in arr[1:] if x <= arr[0]]) + \
        [arr[0]] + quicksort([x for x in arr[1:] if x > arr[0]])


if __name__ == '__main__':
    print(quicksort([9, 8, 7, 3, 21, 2, 4, 1, 1, 35]))
