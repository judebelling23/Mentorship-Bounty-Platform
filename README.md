# 🎯 Mentorship Bounty Platform

A decentralized platform built on Stacks blockchain that connects mentors and mentees through verified knowledge transfer sessions with secure payments.

## 🚀 Features

- 👨‍🏫 **Mentor Profiles**: Create profiles with expertise areas and hourly rates
- 👨‍🎓 **Mentee Profiles**: Track learning progress and session history  
- 💰 **Bounty System**: Post bounties for specific mentorship sessions
- 🔍 **Application Process**: Mentors can apply with custom proposals
- ✅ **Verification System**: Dual verification by both mentor and mentee
- 🛡️ **Dispute Resolution**: Automatic refunds for unverified sessions
- 💸 **Secure Payments**: Escrowed payments with platform fees

## 📋 Contract Functions

### Profile Management
- `create-mentor-profile` - Register as a mentor with expertise and rates
- `create-mentee-profile` - Register as a mentee to access platform

### Bounty Lifecycle
- `create-bounty` - Post a new mentorship bounty with STX payment
- `apply-for-bounty` - Mentors submit applications with proposals
- `accept-mentor` - Mentees select their preferred mentor
- `complete-session` - Mentors mark sessions as completed
- `verify-session` - Mentees verify and release payment
- `dispute-session` - Handle unverified sessions after deadline
- `cancel-bounty` - Cancel open bounties and get refunds

### Read Functions
- `get-bounty` - Retrieve bounty details
- `get-mentor-profile` - View mentor information
- `get-mentee-profile` - View mentee statistics
- `get-session-verification` - Check verification status

## 🎮 Usage Examples

### 1. Create Mentor Profile
```clarity
(contract-call? .mentorship-bounty create-mentor-profile 
  "Alice Smith" 
  "Blockchain Development, Smart Contracts" 
  u50000000)
```

### 2. Create Mentee Profile  
```clarity
(contract-call? .mentorship-bounty create-mentee-profile "Bob Johnson")
```

### 3. Post a Bounty
```clarity
(contract-call? .mentorship-bounty create-bounty
  "Learn Smart Contract Security"
  "Need 2-hour session on common vulnerabilities and best practices"
  u100000000)
```

### 4. Apply for Bounty
````clarity
(contract-call? .mentorship-bounty apply-for-bounty
