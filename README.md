# 💰 Decentralized Salary Streaming

A smart contract system for streaming sBTC payments to employees on the Stacks blockchain. Enable continuous, automated salary payments with hourly or daily streaming rates.

## ✨ Features

- 🔄 **Continuous Streaming**: Stream payments hourly or daily
- 💎 **sBTC Integration**: Native Stacks token streaming
- ⚡ **Instant Claims**: Employees can claim accrued payments anytime
- 🛡️ **Secure**: Built-in authorization and balance checks
- 📊 **Analytics**: Track stream progress and statistics
- 📱 **Responsive UI**: Works on desktop and mobile

## 🚀 Quick Start

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Node.js and npm
- Stacks wallet for testing

### Installation

1. Clone the repository
```bash
git clone https://github.com/your-username/decentralized-salary-streaming.git
cd decentralized-salary-streaming
```

2. Install dependencies
```bash
npm install
```

3. Run tests
```bash
clarinet test
```

4. Start the UI
```bash
cd ui
# Open index.html in your browser
```

## 📋 Contract Functions

### 👔 Employer Functions

#### Create Hourly Stream
```clarity
(create-hourly-stream employee-address hourly-rate duration-hours)
```
- **employee-address**: Principal address of the employee
- **hourly-rate**: Payment rate per hour in µSTX
- **duration-hours**: Stream duration in hours

#### Create Daily Stream
```clarity
(create-daily-stream employee-address daily-rate duration-days)
```
- **employee-address**: Principal address of the employee
- **daily-rate**: Payment rate per day in µSTX
- **duration-days**: Stream duration in days

#### Pause Stream
```clarity
(pause-stream stream-id)
```
Temporarily pause a stream (can be resumed later)

#### Resume Stream
```clarity
(resume-stream stream-id)
```
Resume a paused stream

#### Cancel Stream
```clarity
(cancel-stream stream-id)
```
Cancel a stream and return remaining balance

### 👨‍💼 Employee Functions

#### Claim Payment
```clarity
(claim-payment stream-id)
```
Claim accrued payments from a stream

### 📊 Read-Only Functions

#### Get Stream Details
```clarity
(get-stream stream-id)
```
Returns complete stream information

#### Get Claimable Amount
```clarity
(get-claimable-amount stream-id)
```
Returns amount available for claiming

#### Get User Streams
```clarity
(get-user-streams user-address user-type)
```
Returns list of streams for a user ("employer" or "employee")

#### Get Stream Progress
```clarity
(get-stream-progress stream-id)
```
Returns stream completion percentage and blocks elapsed

## 🌐 Web Interface

The project includes a responsive web interface with three main sections:

### 👔 Employer Dashboard
- Create new salary streams
- View and manage existing streams
- Pause/resume/cancel streams
- Track stream progress

### 👨‍💼 Employee Dashboard
- View incoming streams
- Claim available payments
- Track payment history

### 📊 Analytics Dashboard
- Platform statistics
- Recent activity feed
- Stream volume metrics

## 🔧 Technical Details

### Stream Mechanics
- **Hourly Rate**: 1 Stacks block ≈ 10 minutes, 6 blocks per hour
- **Daily Rate**: 144 blocks per day
- **Block-based Calculation**: Payments calculated per block for precision
- **Claim Anytime**: Employees can claim accrued amounts at any time

### Security Features
- ✅ Authorization checks for all actions
- ✅ Balance validation before transfers
- ✅ Stream state management
- ✅ Overflow protection
- ✅ Reentrancy protection

### Gas Optimization
- Efficient data structures
- Minimal storage operations
- Batch processing where possible
- Optimized calculation functions

## 📱 Mobile Support

The UI is fully responsive and works on:
- 📱 Mobile phones (iOS/Android)
- 💻 Tablets
- 🖥️ Desktop computers

## 🧪 Testing

Run the test suite:
```bash
clarinet test
```

Test coverage includes:
- Stream creation and management
- Payment claiming logic
- Authorization checks
- Edge cases and error handling

## 🔮 Future Enhancements

- 🎯 **Multi-token Support**: Support for other Stacks tokens
- 📈 **Yield Integration**: Auto-compound streaming payments
- 🔔 **Notifications**: Email/SMS alerts for payments
- 🌍 **Multi-chain**: Cross-chain salary streaming
- 📊 **Advanced Analytics**: Detailed financial reporting

## 📄 License

MIT License - see LICENSE file for details

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📞 Support

- 🐛 **Issues**: [GitHub Issues](https://github.com/your-username/decentralized-salary-streaming/issues)
- 💬 **Discussions**: [GitHub Discussions](https://github.com/your-username/decentralized-salary-streaming/discussions)
- 📧 **Email**: support@example.com

## 🎉 Acknowledgments

- Built with [Clarinet](https://github.com/hirosystems/clarinet)
- Powered by [Stacks Blockchain](https://stacks.co)
- UI inspired by modern DeFi interfaces

---

**⚡ Start streaming salaries today!** 🚀
