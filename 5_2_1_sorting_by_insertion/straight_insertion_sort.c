for (i = 1; i < n; i++) {
    tmp = data[i];
    if (data[i - 1] > tmp) {
        j = i;
        do {
            data[j] = data[j - 1];
            j--;
        } while (j > 0 && data[j - 1] > tmp);
        data[j] = tmp;
    }
}

// 8 4 3 7 6 5 2 1 (initial data)
// 4 8 3 7 6 5 2 1 (after 1st loop)
// 3 4 8 7 6 5 2 1 (after 2nd loop)
// 3 4 7 8 6 5 2 1 (after 3rd loop)
// 3 4 6 7 8 5 2 1 (after 4th loop)
// 3 4 5 6 7 8 2 1 (after 5th loop)
// 2 3 4 5 6 7 8 1 (after 6th loop)
// 1 2 3 4 5 6 7 8 (after 7th loop. sorted)
