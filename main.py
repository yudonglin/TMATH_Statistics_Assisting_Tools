from module import Person,compare

Charlie = Person("Charlie",23.7,0.770,1.22)
Amy = Person("Amy",12.5,2.70,-0.25)

compare(Charlie,Amy)
#print(Charlie.current_speed,Amy.current_speed)
for i in range(8):
    print(Amy.cal_position(i))