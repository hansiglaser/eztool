/***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#include <stdbool.h>
#include <stdint.h>

#include "commands.h"
#include "common.h"
#include "usb.h"
#include "i2c.h"
#include "io.h"

// I2C addresses
#define I2C_ADDR_EEPROM 0x50   // 24C00

// local copy of the information we got in the SETUPDAT packet
volatile uint8_t  Command;
volatile uint16_t CmdIndex;
volatile uint16_t CmdValue;

/****************************************************************************/
/***  GetVersion  ***********************************************************/
/****************************************************************************/

const char __code const * Version = "EZ-Tools 0.1";

void GetVersion() {
  uint8_t b;
  __code char* Src;
  __xdata char* Dst;

  Src = Version;
  Dst = IN2BUF;
  b = 0;
  while (*Src) {
    *Dst++ = *Src++;
    b++;
  }
  IN2BC = b;
}

/****************************************************************************/
/***  SetupIOPort  **********************************************************/
/****************************************************************************/

void SetupIOPort() {
  switch (CmdIndex & 0x00FF) {
    case 0: {
      PORTACFG = CmdValue & 0x00FF;
      OEA      = CmdValue >> 8;
      break;
    }
    case 1: {
      PORTBCFG = CmdValue & 0x00FF;
      OEB      = CmdValue >> 8;
      break;
    }
    case 2: {
      PORTCCFG = CmdValue & 0x00FF;
      OEC      = CmdValue >> 8;
      break;
    }
  }
}

/****************************************************************************/
/***  SetIOPort  ************************************************************/
/****************************************************************************/

void SetIOPort() {
  switch (CmdIndex & 0x00FF) {
    case 0: {
      OUTA = CmdValue & 0x00FF;
      break;
    }
    case 1: {
      OUTB = CmdValue & 0x00FF;
      break;
    }
    case 2: {
      OUTC = CmdValue & 0x00FF;
      break;
    }
  }
}

/****************************************************************************/
/***  GetIOPort  ************************************************************/
/****************************************************************************/

void GetIOPort() {
  switch (CmdIndex & 0x00FF) {
    case 0: {
      IN2BUF[0] = PINSA;
      IN2BC = 1;
      break;
    }
    case 1: {
      IN2BUF[0] = PINSB;
      IN2BC = 1;
      break;
    }
    case 2: {
      IN2BUF[0] = PINSC;
      IN2BC = 1;
      break;
    }
  }
}

/****************************************************************************/
/***  ReadEEPROM  ***********************************************************/
/****************************************************************************/

// CmdIndex: Start Address
// CmdValue: Length
void ReadEEPROM() {
  __xdata uint8_t Addr;
  uint8_t Len;
  // get parameters
  Addr = CmdIndex & 0x00FF;
  Len  = CmdValue & 0x00FF;
  // 1 <= Length <= 64 (because of IN2BUF)
  if (Len == 0) return;
  if (Len > 64) return;
  // send start address
  if (i2c_write(I2C_ADDR_EEPROM,1,&Addr) != I2C_OK) {
    // ERROR
    return;
  }
  // read
  if (i2c_read(I2C_ADDR_EEPROM,Len,IN2BUF) != I2C_OK) {
    // ERROR
    return;
  }
  IN2BC = Len;
}

/****************************************************************************/
/***  WriteEEPROM  **********************************************************/
/****************************************************************************/

typedef struct {
  uint8_t Addr;
  uint8_t Data[16];   // maximum 16 bytes per page
} WriteEEPROM_t;

// CmdIndex: Start Address
// CmdValue: Length
void WriteEEPROM() {
  __xdata WriteEEPROM_t Data;
  uint8_t Len;
  uint8_t i;
  // get parameters
  Data.Addr = CmdIndex & 0x00FF;
  Len       = CmdValue & 0x00FF;
  // 1 <= Length <= 16 (page size)
  if (Len == 0) return;
  if (((Data.Addr & 0x000F)+Len) > 16) return;
  // copy data
  for (i = 0; i < Len; i++)
    Data.Data[i] = OUT2BUF[i];
  // send address and data  
  if (i2c_write(I2C_ADDR_EEPROM,1+Len,(__xdata uint8_t*)&Data) != I2C_OK) {
    // ERROR
    return;
  }
}

/****************************************************************************/
/***  ReadXDATA  ************************************************************/
/****************************************************************************/

// CmdIndex: Start Address
// CmdValue: Length
void ReadXDATA() {
  uint8_t __xdata *Addr;
  uint8_t Len;
  uint8_t i;

  if (CmdValue == 0) return;  // don't do anything if the length is 0

  Addr = (uint8_t __xdata*)CmdIndex;
  if (CmdValue > 64) {
    Len = 64;
    CmdValue -= 64;
  } else {
    Len = CmdValue;
    CmdValue = 0;
  }

  for (i = 0; i < Len; i++)
    IN2BUF[i] = *Addr++;

  // store next address
  CmdIndex = (uint16_t)Addr;

  IN2BC = Len;
}

/****************************************************************************/
/***  WriteXData  ***********************************************************/
/****************************************************************************/

void WriteXDATA() {
  uint8_t __xdata *Addr;
  uint8_t Len;
  uint8_t i;

  Addr = (uint8_t __xdata*)CmdIndex;
  Len = OUT2BC;
  if (Len > CmdValue) {   // limit length
    Len = CmdValue;
  }
  CmdValue -= Len;

  for (i = 0; i < Len; i++)
    *Addr++ = OUT2BUF[i];

  // store next address
  CmdIndex = (uint16_t)Addr;

  OUT2BC = 0;
}

/****************************************************************************/
/***  ReadI2C  **************************************************************/
/****************************************************************************/

// CmdIndex: I2C Address
// CmdValue: Length
void ReadI2C() {
  uint8_t Addr;
  uint8_t Len;
  // get parameters
  Addr = CmdIndex & 0x00FF;
  Len  = CmdValue & 0x00FF;
  // 1 <= Length <= 64 (because of IN2BUF)
  if (Len == 0) return;
  if (Len > 64) return;
  // read
  if (i2c_read(Addr,Len,IN2BUF) != I2C_OK) {
    // ERROR
    return;
  }
  IN2BC = Len;
}

/****************************************************************************/
/***  WriteI2C  *************************************************************/
/****************************************************************************/

// CmdIndex: I2C Address
// CmdValue: Length
void WriteI2C() {
  uint8_t Addr;
  uint8_t Len;
  // get parameters
  Addr = CmdIndex & 0x00FF;
  Len  = CmdValue & 0x00FF;
  // 1 <= Length <= 64 (because of OUT2BUF)
  if (Len == 0) return;
  if (Len > 64) return;
  // write
  if (i2c_write(Addr,Len,OUT2BUF) != I2C_OK) {
    // ERROR
    return;
  }
}

/****************************************************************************/
/***  Command Handler  ******************************************************/
/****************************************************************************/

/**
 * Command Handler
 *
 * This function is executed from command_loop() if its semaphore is set.
 */
void HandleCmd() {
  if ((setup_data.bmRequestType & ~USB_DIR_IN) != (USB_REQ_TYPE_VENDOR | USB_RECIP_DEVICE)) {
    return;
  }
  // save command
  Command  = setup_data.bRequest;
  CmdIndex = setup_data.wIndex;
  CmdValue = setup_data.wValue;
  switch (Command) {
    case CMD_GET_VERSION: { // Get Version ////////////////////////////////////
      GetVersion();
      break;
    }
    case CMD_SETUP_IOPORT: {   // PORTxCFG and OEx ////////////////////////////
      SetupIOPort();
      break;
    }
    case CMD_SET_IOPORT: {     // write OUTx //////////////////////////////////
      SetIOPort();
      break;
    }
    case CMD_GET_IOPORT: {     // read INx ////////////////////////////////////
      GetIOPort();
      break;
    }
    case CMD_READ_EEPROM: {    // read from EEPROM ////////////////////////////
      ReadEEPROM();
      break;
    }
    case CMD_WRITE_EEPROM: {   // write to EEPROM /////////////////////////////
      // arm EP2
      OUT2BC = 0;
      // wait for EP2 Sempaphore, rest is done in HandleOut()
      break;
    }
    case CMD_READ_XDATA: {     // read from XDATA memory //////////////////////
      ReadXDATA();
      break;
    }
    case CMD_WRITE_XDATA: {    // write to XDATA memory ///////////////////////
      // arm EP2
      OUT2BC = 0;
      // wait for EP2 Sempaphore, rest is done in HandleOut()
      break;
    }
    case CMD_READ_I2C: {       // generic read at I2C bus /////////////////////
      ReadI2C();
      break;
    }
    case CMD_WRITE_I2C: {      // generic write at I2C bus ////////////////////
      // arm EP2
      OUT2BC = 0;
      // wait for EP2 Sempaphore, rest is done in HandleOut()
      break;
    }
    default: {
      break;
    }
  }
}

/**
 * EP IN Interrupt handler
 *
 * This function is executed from main() if its semaphore is set.
 */
void HandleIn() {
  switch (Command) {
    case CMD_READ_XDATA: {     // read from XDATA memory //////////////////////
      ReadXDATA();
      break;
    }
    default: {
      break;
    }
  }
}

/**
 * EP OUT Interrupt handler
 *
 * This function is executed from main() if its semaphore is set.
 */
void HandleOut() {
  switch (Command) {
    case CMD_WRITE_EEPROM: {   // write to EEPROM /////////////////////////////
      WriteEEPROM();
      break;
    }
    case CMD_WRITE_XDATA: {    // write to XDATA memory ///////////////////////
      WriteXDATA();
      break;
    }
    case CMD_WRITE_I2C: {      // generic write at I2C bus ////////////////////
      WriteI2C();
      break;
    }
    default: {
      break;
    }
  }
}

/**
 * Main command loop
 *
 * This function has an infinite loop and does not return.
 *
 */
void command_loop(void) {
  // arm EP2OUT for the first time so we are ready for JTAG commands
  OUT2BC = 0;
  // command loop
  while (true) {
    // got a command packet?
    if (Semaphore_Command) {
      HandleCmd();
      Semaphore_Command = false;
    }
    // got an EP2 IN interrupt?
    if (Semaphore_EP2_in) {
      HandleIn();
      Semaphore_EP2_in = false;
    }
    // got an EP2 OUT interrupt?
    if (Semaphore_EP2_out) {
      HandleOut();
      Semaphore_EP2_out = false;
    }
  }
}
