# Smart Parking Token System

A decentralized parking management system built on Stacks blockchain using Clarity smart contracts.

## Overview

This smart contract enables a decentralized parking system where:

- Parking space owners can register their spaces
- Users can pay for parking using STX tokens
- The system tracks parking sessions, duration, and payments
- Users can extend their parking time as needed

## Contract Functions

### Admin Functions

- `register-parking-space`: Register a new parking space with location and rate
- `update-space-availability`: Change availability status of a parking space
- `update-hourly-rate`: Update the default hourly rate for parking

### User Functions

- `park-vehicle`: Pay for and start a parking session
- `end-parking`: End an active parking session
- `extend-parking`: Add more time to an existing parking session

### Read-Only Functions

- `get-hourly-rate`: Get the current default hourly rate
- `get-total-revenue`: Get total revenue collected by the system
- `get-total-spaces`: Get total number of registered parking spaces
- `get-parking-space`: Get details about a specific parking space
- `get-active-parking`: Get details about active parking in a space
- `get-user-history`: Get a user's parking history
- `is-space-available`: Check if a parking space is available
- `calculate-parking-fee`: Calculate fee for parking duration

## Usage Examples

### Register a new parking space (admin only)
```clarity
(contract-call? .smart-parking register-parking-space u1 "Downtown Lot A-12" u15)
```

### Park a vehicle
```clarity
;; Park for 2 hours in space #1
(contract-call? .smart-parking park-vehicle u1 u2)
```

### End parking session
```clarity
(contract-call? .smart-parking end-parking u1)
```

### Extend parking time
```clarity
;; Add 1 more hour to current parking session
(contract-call? .smart-parking extend-parking u1 u1)
```

## Error Codes

- `u100`: Not the contract owner
- `u101`: Item not found
- `u102`: Unauthorized action
- `u103`: Already registered
- `u104`: Insufficient funds
- `u105`: Space occupied
- `u106`: Not parked
- `u107`: Invalid duration
- `u108`: Space not registered
```
