# Program Examples in Mungo
- [File Example](#file-example)
- [House Controller Example](#house-controller-example)
- [Bank Account Example](#bank-account-example)
- [Travel Agency Example](#travel-agency-example)

## File Example

| ![File protocol](https://github.com/MungoTypesystem/MungoBehaviouralSeparation/raw/master/protocol_figures/file_protocol.png) | 
|:--:| 
| *Protocol for File class* |

The classic example for typestate programming. A file follows a simple protocol. A file can only be read after it has been opened, and only while the file contains more data than what has already been read. The figure above illustrates this protocol, which can also be seen, expressed as a usage, in the code below.

The implementation of the File class itself is only a skeleton implementation, but the usage is specified according to an actual file implementation. So the interesting class is the main class which actually reads a file. Omitting the line `f.open()` would cause a type error, as would removing the line `f.close()`. 

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
 <p style="display: inline;">Output</p>
</summary>

```
$ mungob exampleprograms/file.mg < datafile.txt

file
with
multiple
lines
```


</details>


## House Controller Example

| ![House controller protocol](https://github.com/MungoTypesystem/MungoBehaviouralSeparation/raw/master/protocol_figures/house_protocol.png) |
|:--:| 
| *Protocol for HouseController class* |

Here we present a more complex example of a class composed of multiple unlrelated fields. Parallel usages allows for the specfication of a protocol with unrelated parts. In this example, each field results in a parallel constituent for a local protocol for using that specific field. The parallel usages avoid enumerating the exponential number of states arising when allowing arbritrary interleaving of methods on unrelated fields without parallel usages. The figure above illustrates the parallel usage, where each local protocol is followed simultaneously. 

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

    void initLightController() { lc = new LightController }

    void initTempController() { tc = new TempController }

    void initDoorController() { dc = new DoorController }

    void lightOn() { lc.on() }

    void adjustLight() { lc.changeIntensity() }

    void lightOff() { lc.off() }

    void setTemperature(int x) { tc.setTemperature(x) }

    void turnTempOff() { tc.off() }

    void lockDoors() { dc.lock() }

    void unlockDoors() { dc.unlock() }

    void turnDoorOff() { dc.off() }
}
```

## Bank Account Example

| ![Account protocol](https://github.com/MungoTypesystem/MungoBehaviouralSeparation/raw/master/protocol_figures/account_protocol.png) |
|:--:| 
| *Protocol for Account class* |

In this example we present the sequential usage, and show how a usage U;U'=(U|end).U' can be used to simplify method parameter types, and allow a form of method overloading. The figure above shows the protocol for a bank account. The balance of the account cannot be accessed after calling `addSalary` but before `applyInterest`, hence we never access an intermediate account balance. By using behavioural separation with the sequential usage, the `output` method can be used when `account` has type `Account[{getBalance; end};{addSalary; {applyInterest; end}};{getBalance; end}]` and `Account[{getBalance; end}]`. 

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
  <p style="display: inline;">Output</p>
 </summary>

```
$ mungob exampleprograms/account.mg


0
16030
```

</details>

## Travel Agency Example

| ![Agency protocol](https://github.com/MungoTypesystem/MungoBehaviouralSeparation/raw/master/protocol_figures/agency_protocol.png) |
|:--:| 
| *Protocol for Agency class* |

We present another classic example, this time from session type theory. A customer must contact a travel agency and book a trip, after bargaining for a price that is acceptable. The protocol for the travel agent is shown above, and shows a quote can be continuosly requested until the price is reasonable, in which case the trip is accepted, and the travel service is contacted and given the buyers information, and a date for the trip is returned. 

```java
class PriceValidator[{isFairPrice; <end, end>}] {
    bool isFairPrice(int x) {
        (x <= 100)
    }
}

class Customer[{bargain; {finalize; end}}] {
    int price
    PriceValidator pv
    Service s
    int date

    void bargain(Agency[{getQuote; rec X.{getQuote; X accept; end}}] -> Agency[end] agent) {
        pv = new PriceValidator;
        price = agent.getQuote();
        if (pv.isFairPrice(price)) {
            print("Initial price accepted");
            print(price);
            s = agent.accept()
        } else {
            pv = new PriceValidator;
            loop: (
                price = agent.getQuote();
                print("New price received from agency");
                print(price);
                if (pv.isFairPrice(price)) {
                    print("Accepted price");
                    print(price);
                    s = agent.accept()
                } else {
                    pv = new PriceValidator;
                    continue loop
                }
            )
        }
        
    }

    void finalize() {
        s.setAddress("Selma Lagerlöfs Vej 300");
        date = s.getDate()
    }
}

class Agency[{init; {getQuote; rec X.{getQuote; X accept; end}}}] {
    int curQuote

    void init() {
        curQuote = (210)
    }

    int getQuote() {
        curQuote = (curQuote - 10);
        curQuote
    }

    Service[{setAddress; {getDate; end}}] accept() {
        new Service
    }

}

class Service[{setAddress; {getDate; end}}] {
    string address
    void setAddress(string x) {
        address = x
    }

    int getDate() {27}
}

class main[{main; end}] {
    Customer c
    Agency a
    
    void main() {
        c = new Customer;
        a = new Agency;
        a.init();
        c.bargain(a);
        c.finalize()
    }
}
```

<details>
 <summary>
  <p style="display: inline;">Output</p>
 </summary>

```
$ mungob exampleprograms/travel.mg


New price received from agency
190
New price received from agency
180
New price received from agency
170
New price received from agency
160
New price received from agency
150
New price received from agency
140
New price received from agency
130
New price received from agency
120
New price received from agency
110
New price received from agency
100
Accepted price
100
```

</details>
