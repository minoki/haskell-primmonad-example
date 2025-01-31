#include <stdio.h>

void putint(int n)
{
    if (n == 0) {
        putchar('0');
        return;
    } else if (n < 0) {
        putchar('-');
        n = -n;
    }
    char buf[10];
    char *p = buf;
    while (n != 0) {
        *p++ = '0' + (n % 10);
        n = n / 10;
    }
    while (p != buf) {
        putchar(*--p);
    }
}

int main(void)
{
    for (int i = 1; i <= 15; i++) {
        if (i % 5) {
            if (i % 3) {
                // printf("%d\n", i);
                putint(i);
                putchar('\n');
            } else {
                puts("Fizz");
            }
        } else {
            puts("FizzBuzz" + i * i % 3 * 4);
        }
    }
    return 0;
}
