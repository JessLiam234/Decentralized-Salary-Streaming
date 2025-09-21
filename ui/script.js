document.addEventListener('DOMContentLoaded', function() {
    initializeTabs();
    initializeStreamTypeChange();
    loadMockData();
});

function initializeTabs() {
    const tabButtons = document.querySelectorAll('.tab-button');
    const tabContents = document.querySelectorAll('.tab-content');

    tabButtons.forEach(button => {
        button.addEventListener('click', () => {
            const targetTab = button.getAttribute('data-tab');
            
            tabButtons.forEach(btn => btn.classList.remove('active'));
            tabContents.forEach(content => content.classList.remove('active'));
            
            button.classList.add('active');
            document.getElementById(targetTab).classList.add('active');
        });
    });
}

function initializeStreamTypeChange() {
    const streamTypeSelect = document.getElementById('stream-type');
    const durationLabel = document.getElementById('duration-label');
    
    streamTypeSelect.addEventListener('change', function() {
        durationLabel.textContent = this.value === 'hourly' ? 'hours' : 'days';
    });
}

function createStream() {
    const employeeAddress = document.getElementById('employee-address').value;
    const streamType = document.getElementById('stream-type').value;
    const rate = document.getElementById('rate').value;
    const duration = document.getElementById('duration').value;
    
    if (!employeeAddress || !rate || !duration) {
        alert('Please fill in all fields');
        return;
    }
    
    if (!employeeAddress.startsWith('SP')) {
        alert('Please enter a valid Stacks address');
        return;
    }
    
    const streamData = {
        employeeAddress,
        streamType,
        rate: parseInt(rate),
        duration: parseInt(duration)
    };
    
    console.log('Creating stream:', streamData);
    
    alert(`Stream created successfully!\n\nEmployee: ${employeeAddress}\nType: ${streamType}\nRate: ${rate} µSTX\nDuration: ${duration} ${streamType === 'hourly' ? 'hours' : 'days'}`);
    
    document.getElementById('employee-address').value = '';
    document.getElementById('rate').value = '';
    document.getElementById('duration').value = '';
}

function loadMockData() {
    updateStats();
    setInterval(updateClaimableAmounts, 5000);
}

function updateStats() {
    const stats = [
        { value: Math.floor(Math.random() * 200) + 100, label: 'Total Streams' },
        { value: (Math.random() * 5 + 1).toFixed(1) + 'M', label: 'µSTX Streamed' },
        { value: Math.floor(Math.random() * 50) + 50, label: 'Active Users' },
        { value: (Math.random() * 2 + 97).toFixed(1) + '%', label: 'Uptime' }
    ];
    
    const statCards = document.querySelectorAll('.stat-card');
    statCards.forEach((card, index) => {
        if (stats[index]) {
            card.querySelector('.stat-value').textContent = stats[index].value;
        }
    });
}

function updateClaimableAmounts() {
    const claimableElements = document.querySelectorAll('.claimable');
    claimableElements.forEach(element => {
        const currentAmount = parseInt(element.textContent.replace(/[^\d]/g, ''));
        const newAmount = currentAmount + Math.floor(Math.random() * 100) + 50;
        element.textContent = newAmount.toLocaleString() + ' µSTX';
    });
}

function pauseStream(streamId) {
    console.log('Pausing stream:', streamId);
    alert('Stream paused successfully');
}

function resumeStream(streamId) {
    console.log('Resuming stream:', streamId);
    alert('Stream resumed successfully');
}

function cancelStream(streamId) {
    if (confirm('Are you sure you want to cancel this stream?')) {
        console.log('Cancelling stream:', streamId);
        alert('Stream cancelled successfully');
    }
}

function claimPayment(streamId) {
    const claimableElement = document.querySelector('.claimable');
    const amount = claimableElement.textContent.replace(/[^\d]/g, '');
    
    console.log('Claiming payment for stream:', streamId, 'Amount:', amount);
    
    alert(`Payment claimed successfully!\nAmount: ${parseInt(amount).toLocaleString()} µSTX`);
    
    claimableElement.textContent = '0 µSTX';
}

window.createStream = createStream;
window.pauseStream = pauseStream;
window.resumeStream = resumeStream;
window.cancelStream = cancelStream;
window.claimPayment = claimPayment;
