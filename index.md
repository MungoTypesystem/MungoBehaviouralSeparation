# Program Examples in Mungo

- [Program Examples in Mungo](#program-examples-in-mungo)
  * [File Example](#file-example)
  * [House Controller Example](#house-controller-example)
  * [Bank Account Example](#bank-account-example)

## File Example
![File protocol](https://github.com/MungoTypesystem/MungoBehaviouralSeparation/raw/master/protocol_figures/file_protocol.png)
```java
class File [{open; rec X.{isEmpty; <{close; end}, {read; X}>}}] {

    string next
    void open() {
        unit
    }
    
    bool isEmpty() {
        next = input();
        next == ""
    }

    string read() {
        next
    }

    void close() {
        unit
    }
}



class main[{main; end}] {
    File f
    string lastLine    
    void main() {
        f = new File;
        f.open();
        (loop: 
            if (f.isEmpty()) {
                f.close()
            } else {
                lastLine = f.read();
                print(lastLine);
                continue loop
            }
        )
    }
}
```
<details>
<summary>
 Output
</summary>
<p>

```bash
$ mungob exampleprograms/file.mg < datafile.txt                                              

file
with
multiple
lines
 ```

</p>
</details>


## House Controller Example
![House controller protocol](https://github.com/MungoTypesystem/MungoBehaviouralSeparation/raw/master/protocol_figures/house_protocol.png)
```java
class LightController[
        { on; rec Y.{changeIntensity; Y off; end} }
    ] {
    void on() {
        print("LightController turned on")
    }
    void off() {
        print("LightController turned off")
    }
    void changeIntensity() {
        print("LightController set to high intensity")
    }
}

class TempController [
        rec A.{
            setTemperature; A
            off; end
        }
    ] {
    void off() {
        print("TempController turned off")
    }
    void setTemperature(int temp) {
        print("TempController set to high intensity");
        print(temp)
    }
}

class DoorController [
        rec B.{
            lock; B
            unlock; B
            off; end
        }
    ] {
    void off() {
        print("DoorController turned off")
    }
    
    void lock() {
        print("DoorController locked doors")
    }

    void unlock() {
        print("DoorController unlocked doors")
    }
}

class HouseController [
    (
    { initLightController; 
        {lightOn; rec C.{ 
            adjustLight; C 
            lightOff; end}}}
    |
    { initTempController; 
        rec D. {
            setTemperature; D
            turnTempOff; end}}
    |
    { initDoorController; 
        rec E. {
            lockDoors; E
            unlockDoors; E
            turnDoorOff; end }} 
    ).end 
    ] {
    LightController lc
    TempController tc
    DoorController dc

    void initLightController() {
        lc = new LightController
    }

    void initTempController() {
        tc = new TempController
    }

    void initDoorController() {
        dc = new DoorController
    }

    void lightOn() {
        lc.on()
    }

    void adjustLight() {
        lc.changeIntensity()
    }

    void lightOff() {
        lc.off()
    }

    void setTemperature(int x) {
        tc.setTemperature(x)
    }

    void turnTempOff() {
        tc.off()
    }

    void lockDoors() {
        dc.lock()
    }

    void unlockDoors() {
        dc.unlock()
    }

    void turnDoorOff() {
        dc.off()
    }
}
```

## Bank Account Example
![Account protocol](https://github.com/MungoTypesystem/MungoBehaviouralSeparation/raw/master/protocol_figures/account_protocol.png)
```java
class Account [
        {getBalance; end};{addSalary; {applyInterest; end}};{getBalance; end}
    ] {

    int balance

    int getBalance() { balance }
    void addSalary() { balance = (balance + 16000) }
    void applyInterest() { balance = (balance + 30) }
}

class Printer [rec X.{output; X finish; end}] {
    int balance
    void output(Account[{getBalance; end}] -> Account[end] x) {
        balance = x.getBalance();
        print(balance)
    }
    void finish() { unit }
}

class main [{main; end}] {
    Account acc
    Printer p

    void main() {
        acc = new Account;
        p = new Printer;
        p.output(acc);
        acc.addSalary();
        acc.applyInterest();
        p.output(acc);
        p.finish()
    }
}
```

<details>
 <summary>
  Output
 </summary>
<p>

```bash
$ mungob exampleprograms/account.mg                                             


0
16030
```

</p>
</details>

