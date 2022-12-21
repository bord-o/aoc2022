## wtf is going on here...

#### pretending that parsing is already done

when our filesystem construction function sees

```bash
$ cd dirname
```
need to return a directory constructor where the internal
list contains the recursive call

```bash
$ cd ..
```

this is tricky. We return nothing?

```bash
$ ls
```

i dont think we need this for anything, maybe filter it off completely


```bash
number filename
```

need to return a file object with the size and names set


```bash
dir dirname
```

new directories arent handled until we 'cd' into them so this can be skipped
i think


Dir /
  File b.txt
  File c.dat
  Dir a
    File f
    File g
    File h.lst
    Dir e
      file i

