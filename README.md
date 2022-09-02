# STATES Driver for ACSys/FE

This driver provides the STATES service for the Fermilab (and NOvA)
control system. The set of ACNET devices associated with this driver
are known as "state devices". They take a 16-bit value for their
setting. Each time a state device is set, its value is multicast.
ACNET clients (through other services) can receive these multicasts.

The last value set on a state device can be read.

State devices can be updated in the following ways:

- A 16-bit setting can be made. The control system will route the
  setting to this driver. The driver assumes it owns the device and
  will add it to its table. This means adding a new state device to
  the database doesn't require this driver to reload any
  configuration; just set the new device.

- An alternate ACNET service (FSMSET) is available. I'm not sure why
  this interface was needed when standard ACNET settings work. But it
  was available on the MOOC states front-end, so this driver supports
  it.

## NOTE

Unlike the MOOC states front-end, this driver doesn't do any database
access to determine the set of DIs it owns. Nor does it try to keep in
sync when new devices are added. Simply setting the device will add
the DI to its set. This means after a restart the driver starts with
an empty set. Trying to read a state device after a restart, but
before the device gets set will return an error.

This strategy has woked well at NOvA (which has only used this driver
for their STATES support.)

We may add an RPC in the LOOPUP service to provide a way to initialize
the driver (without requiring the driver to do any SQL.) But that's
still being discussed.
