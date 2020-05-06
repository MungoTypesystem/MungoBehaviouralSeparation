class File {
    {open; rec X.{isEmpty; <{close; end}, {readChar; X}>}}


    bool isEmpty(void->void x, void->void x2) {
        true
    }

    bool readChar(void->void x, void->void x2) {false}
    void close(void->void x, void->void x2) {unit}
    void open(void->void x, void->void x2) {unit}
}

class FileReader {
    {readFile; end}

    File f

    void readFile(void->void x, void->void x2) {
        f = new File;
        f.open(unit, unit);
        loop: if (f.isEmpty(unit, unit)) {
                f.close(unit, unit)
              } else {
                 f.readChar(unit, unit);
                 continue loop
              }
    }
}
