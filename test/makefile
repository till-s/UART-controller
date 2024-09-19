GHDL=ghdl

GHDL_A_FLAGS=-g
GHDL_E_FLAGS=-g -Wl,-no-pie

TESTS=UartTb LongDividerTb

VPATH=../hdl

SRCS=UartPkg.vhd
SRCS+=UartTx.vhd
SRCS+=UartRx.vhd
SRCS+=RateGenerator.vhd
SRCS+=UartTb.vhd

SRCS+=LongDivider.vhd
SRCS+=LongDividerTb.vhd

test: build
	for p in $(TESTS); do ./$${p}; done && echo "All Tests PASSED"

build: $(TESTS)

UartTb.o: UartTx.o UartRx.o RateGenerator.o UartPkg.o

LongDividerTb.o: LongDivider.o

UartTx.o UartRx.o RateGenerator.o: UartPkg.o

%Tb: %Tb.o
	$(GHDL) -e $(GHDL_E_FLAGS) -o $@ $@

%.o: %.vhd
	$(GHDL) -a $(GHDL_A_FLAGS) $<

clean:
	$(RM) $(SRCS:%.vhd=%.o) $(TESTS) work-obj* e~*.o

.PHONY: clean build test
