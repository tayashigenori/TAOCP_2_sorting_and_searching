static void upheap(double arr[], int n);
static void downheap(double arr[], int n);
 
static inline void
swap(double arr[], int a, int b)
{
    double tmp = arr[a];
    arr[a] = arr[b];
    arr[b] = tmp;
}
 
void
heapsort(double arr[], int n_elems)
{
    int i = 0;
 
    /*
     * arr の先頭から順に、ヒープを成長させる
     *  0    1    2  | 3    4    5
     * [  ] [  ] [  ]|[  ] [  ] [  ]
     *    ヒープ     |   未処理の入力
     *              ===>
     * i は、ヒープ中の要素数であると同時に、未処理データの先頭を
     * 指してもいる
     */
    /* 配列が全部ヒープに入れ替わるまで繰り返す */
    while (++i < n_elems) {
        /*
         * 配列の先頭要素を、ヒープの最後に移動するわけだが、どちらも
         * ちょうど同じ場所に最初からあるので、境界を移動させるだけでよい
         */
 
        /*
         * arr[i] に、ヒープに新しく追加されたデータがあるものとして、
         * 先頭から arr[i] までがヒープになるよう再構成する
         */
        upheap(arr, i);
    }
 
    /*
     * arr の末端から順に、ヒープから取り出して並べる
     *  0    1    2  | 3    4    5
     * [  ] [  ] [  ]|[  ] [  ] [  ]
     *    ヒープ     |   ソート済みの配列
     *             <===
     */
    /* ヒープが全部配列に入れ替わるまで繰り返す */
    while (--i > 0) {
        /*
         * ヒープの先頭要素を、配列に移動すると同時に、ヒープの最後の
         * 要素を、ヒープの先頭に移動する swap
         */
        swap(arr, 0, i);
 
        /*
         * arr[0] に、ヒープの最後から移動されたデータがあるものとして、
         * 先頭から arr[i - 1] までがヒープになるよう再構成する
         */
        downheap(arr, i);
    }
}
 
/*
 * macros for heaptree
 * for 0 origin array
 */
#define LEFT_CHILD(i)  (((i) + 1) * 2 - 1)
#define RIGHT_CHILD(i) (((i) + 1) * 2)
#define PARENT(i)      (((i) + 1) / 2 - 1)
 
/*
 * arr[n] に、ヒープに新しく追加されたデータがあるものとして、
 * 先頭から arr[n] までがヒープになるよう再構成する
 */
static void
upheap(double arr[], int n)
{
    while (n > 0) {
        int m = PARENT(n);
 
        if (arr[m] < arr[n]) {
            swap(arr, m, n);
        } else {
            break;
        }
 
        n = m;
    }
}
 
/*
 * arr[0] に、ヒープの最後から移動されたデータがあるものとして、
 * 先頭から arr[n - 1] までがヒープになるよう再構成する
 */
static void
downheap(double arr[], int n)
{
    int m = 0;
    int tmp = 0;
 
    for (;;) {
        int l_chld = LEFT_CHILD(m);
        int r_chld = RIGHT_CHILD(m);
 
        if (l_chld >= n) {
            break;
        }
 
        if (arr[l_chld] > arr[tmp]) {
            tmp = l_chld;
        }
        if ((r_chld < n) && (arr[r_chld] > arr[tmp])) {
            tmp = r_chld;
        }
 
        if (tmp == m) {
            break;
        }
        swap(arr, tmp, m);
 
        m = tmp;
    }
}