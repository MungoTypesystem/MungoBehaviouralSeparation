# Examples in Mungo
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
