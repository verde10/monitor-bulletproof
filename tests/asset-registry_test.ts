import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v1.0.2/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

// Constants mirroring contract constants
const ROLE_PRODUCER = 1;
const ROLE_CONSUMER = 2;
const ROLE_OPERATOR = 3;

const ERR_UNAUTHORIZED = 401;
const ERR_ALREADY_REGISTERED = 402;
const ERR_PARTICIPANT_NOT_FOUND = 403;
const ERR_INVALID_REGISTRATION = 404;

Clarinet.test({
  name: "Participant Registration: Successful registration for each role type",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const producer = accounts.get('wallet_1')!;
    const consumer = accounts.get('wallet_2')!;
    const operator = accounts.get('wallet_3')!;

    // Test Producer Registration
    let block = chain.mineBlock([
      Tx.contractCall('energy-grid-registry', 'register-participant', [
        types.principal(producer.address),
        types.uint(ROLE_PRODUCER),
        types.uint(100),
        types.utf8('Solar Farm Location')
      ], producer.address)
    ]);
    block.receipts[0].result.expectOk().expectBool(true);

    // Test Consumer Registration
    block = chain.mineBlock([
      Tx.contractCall('energy-grid-registry', 'register-participant', [
        types.principal(consumer.address),
        types.uint(ROLE_CONSUMER),
        types.uint(50),
        types.utf8('Residential Area')
      ], consumer.address)
    ]);
    block.receipts[0].result.expectOk().expectBool(true);

    // Test Operator Registration
    block = chain.mineBlock([
      Tx.contractCall('energy-grid-registry', 'register-participant', [
        types.principal(operator.address),
        types.uint(ROLE_OPERATOR),
        types.uint(200),
        types.utf8('Grid Control Center')
      ], operator.address)
    ]);
    block.receipts[0].result.expectOk().expectBool(true);
  }
});

Clarinet.test({
  name: "Participant Registration: Prevent duplicate registrations",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const user = accounts.get('wallet_1')!;

    // First registration
    let block = chain.mineBlock([
      Tx.contractCall('energy-grid-registry', 'register-participant', [
        types.principal(user.address),
        types.uint(ROLE_PRODUCER),
        types.uint(100),
        types.utf8('Solar Farm Location')
      ], user.address)
    ]);
    block.receipts[0].result.expectOk().expectBool(true);

    // Attempt duplicate registration
    block = chain.mineBlock([
      Tx.contractCall('energy-grid-registry', 'register-participant', [
        types.principal(user.address),
        types.uint(ROLE_PRODUCER),
        types.uint(100),
        types.utf8('Solar Farm Location')
      ], user.address)
    ]);
    block.receipts[0].result.expectErr().expectUint(ERR_ALREADY_REGISTERED);
  }
});

Clarinet.test({
  name: "Authorization Tests: Verify contract owner actions",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const user = accounts.get('wallet_1')!;
    const otherUser = accounts.get('wallet_2')!;

    // First register a participant
    let block = chain.mineBlock([
      Tx.contractCall('energy-grid-registry', 'register-participant', [
        types.principal(user.address),
        types.uint(ROLE_PRODUCER),
        types.uint(100),
        types.utf8('Solar Farm Location')
      ], user.address)
    ]);

    // Unauthorized user attempts status update
    block = chain.mineBlock([
      Tx.contractCall('energy-grid-registry', 'update-participant-status', [
        types.principal(user.address),
        types.bool(false)
      ], otherUser.address)
    ]);
    block.receipts[0].result.expectErr().expectUint(ERR_UNAUTHORIZED);

    // Contract owner successfully updates status
    block = chain.mineBlock([
      Tx.contractCall('energy-grid-registry', 'update-participant-status', [
        types.principal(user.address),
        types.bool(false)
      ], deployer.address)
    ]);
    block.receipts[0].result.expectOk().expectBool(true);
  }
});

Clarinet.test({
  name: "Status Management: Update and verify participant status",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const user = accounts.get('wallet_1')!;

    // Register participant
    let block = chain.mineBlock([
      Tx.contractCall('energy-grid-registry', 'register-participant', [
        types.principal(user.address),
        types.uint(ROLE_PRODUCER),
        types.uint(100),
        types.utf8('Solar Farm Location')
      ], user.address)
    ]);

    // Verify initial status is active
    let activeCheck = chain.callReadOnlyFn(
      'energy-grid-registry', 
      'is-active-participant', 
      [types.principal(user.address)], 
      user.address
    );
    activeCheck.result.expectBool(true);

    // Update status to inactive
    block = chain.mineBlock([
      Tx.contractCall('energy-grid-registry', 'update-participant-status', [
        types.principal(user.address),
        types.bool(false)
      ], deployer.address)
    ]);

    // Verify status update
    activeCheck = chain.callReadOnlyFn(
      'energy-grid-registry', 
      'is-active-participant', 
      [types.principal(user.address)], 
      user.address
    );
    activeCheck.result.expectBool(false);
  }
});

Clarinet.test({
  name: "Error Handling: Invalid registration scenarios",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const user = accounts.get('wallet_1')!;

    // Invalid role
    let block = chain.mineBlock([
      Tx.contractCall('energy-grid-registry', 'register-participant', [
        types.principal(user.address),
        types.uint(4), // Invalid role
        types.uint(100),
        types.utf8('Invalid Location')
      ], user.address)
    ]);
    block.receipts[0].result.expectErr().expectUint(ERR_INVALID_REGISTRATION);

    // Zero energy capacity
    block = chain.mineBlock([
      Tx.contractCall('energy-grid-registry', 'register-participant', [
        types.principal(user.address),
        types.uint(ROLE_PRODUCER),
        types.uint(0), // Zero capacity
        types.utf8('Solar Farm Location')
      ], user.address)
    ]);
    block.receipts[0].result.expectErr().expectUint(ERR_INVALID_REGISTRATION);
  }
});

Clarinet.test({
  name: "Read Operations: Retrieve and validate participant details",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const user = accounts.get('wallet_1')!;

    // Register participant
    let block = chain.mineBlock([
      Tx.contractCall('energy-grid-registry', 'register-participant', [
        types.principal(user.address),
        types.uint(ROLE_PRODUCER),
        types.uint(100),
        types.utf8('Solar Farm Location')
      ], user.address)
    ]);

    // Retrieve participant details
    let details = chain.callReadOnlyFn(
      'energy-grid-registry', 
      'get-participant-details', 
      [types.principal(user.address)], 
      user.address
    );

    // Validate retrieved details
    details.result.expectSome();
    let participantData = details.result.expectSome().expectTuple();
    
    assertEquals(participantData.role, types.uint(ROLE_PRODUCER));
    assertEquals(participantData.wallet, types.principal(user.address));
    assertEquals(participantData.energy-capacity, types.uint(100));
    assertEquals(participantData.location, types.utf8('Solar Farm Location'));
    assertEquals(participantData.status, types.bool(true));
  }
});

Clarinet.test({
  name: "Ownership Transfer: Transfer contract ownership",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const otherUser = accounts.get('wallet_1')!;

    // Successful ownership transfer
    let block = chain.mineBlock([
      Tx.contractCall('energy-grid-registry', 'transfer-ownership', [
        types.principal(otherUser.address)
      ], deployer.address)
    ]);
    block.receipts[0].result.expectOk().expectBool(true);

    // Verify previous owner cannot perform admin actions
    block = chain.mineBlock([
      Tx.contractCall('energy-grid-registry', 'remove-participant', [
        types.principal(otherUser.address)
      ], deployer.address)
    ]);
    block.receipts[0].result.expectErr().expectUint(ERR_UNAUTHORIZED);

    // New owner can now perform admin actions
    block = chain.mineBlock([
      Tx.contractCall('energy-grid-registry', 'remove-participant', [
        types.principal(otherUser.address)
      ], otherUser.address)
    ]);
    block.receipts[0].result.expectOk().expectBool(true);
  }
});