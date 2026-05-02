import Array "mo:base/Array";
import Blob "mo:base/Blob";
import HashMap "mo:base/HashMap";
import Int "mo:base/Int";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Nat8 "mo:base/Nat8";
import Principal "mo:base/Principal";
import Text "mo:base/Text";

persistent actor class ICETToken(initialOwner : Principal, initialSupply : Nat) = this {
  public type Subaccount = Blob;

  public type Account = {
    owner : Principal;
    subaccount : ?Subaccount;
  };

  public type TransferArgs = {
    from_subaccount : ?Subaccount;
    to : Account;
    amount : Nat;
    fee : ?Nat;
    memo : ?Blob;
    created_at_time : ?Nat64;
  };

  public type TransferError = {
    #BadFee : { expected_fee : Nat };
    #BadBurn : { min_burn_amount : Nat };
    #InsufficientFunds : { balance : Nat };
    #TooOld;
    #CreatedInFuture : { ledger_time : Nat64 };
    #Duplicate : { duplicate_of : Nat };
    #TemporarilyUnavailable;
    #GenericError : { error_code : Nat; message : Text };
  };

  public type TransferResult = {
    #Ok : Nat;
    #Err : TransferError;
  };

  public type Value = {
    #Blob : Blob;
    #Text : Text;
    #Nat : Nat;
    #Int : Int;
    #Array : [Value];
    #Map : [(Text, Value)];
  };

  public type SupportedStandard = {
    name : Text;
    url : Text;
  };

  transient let tokenName : Text = "ICEvent Token";
  transient let tokenSymbol : Text = "ICET";
  transient let tokenDecimals : Nat8 = 8;
  transient let transferFee : Nat = 10_000;

  var totalSupply : Nat = initialSupply;
  var nextTxIndex : Nat = 0;

  transient let balances = HashMap.HashMap<Text, Nat>(128, Text.equal, Text.hash);

  func subaccountText(subaccount : Subaccount) : Text {
    Array.foldLeft<Nat8, Text>(
      Blob.toArray(subaccount),
      "",
      func(acc : Text, part : Nat8) : Text {
        if (acc == "") {
          Nat8.toText(part);
        } else {
          acc # "-" # Nat8.toText(part);
        };
      },
    );
  };

  func accountKey(account : Account) : Text {
    Principal.toText(account.owner) # ":" # (switch (account.subaccount) {
      case (null) "";
      case (?sub) subaccountText(sub);
    });
  };

  func getBalance(account : Account) : Nat {
    switch (balances.get(accountKey(account))) {
      case (null) 0;
      case (?balance) balance;
    };
  };

  func putBalance(account : Account, value : Nat) {
    balances.put(accountKey(account), value);
  };

  transient let ownerAccount : Account = {
    owner = initialOwner;
    subaccount = null;
  };

  transient let _ = balances.put(accountKey(ownerAccount), initialSupply);

  public query func icrc1_name() : async Text { tokenName };

  public query func icrc1_symbol() : async Text { tokenSymbol };

  public query func icrc1_decimals() : async Nat8 { tokenDecimals };

  public query func icrc1_fee() : async Nat { transferFee };

  public query func icrc1_total_supply() : async Nat { totalSupply };

  public query func icrc1_minting_account() : async ?Account { ?ownerAccount };

  public query func icrc1_balance_of(account : Account) : async Nat {
    getBalance(account);
  };

  public query func icrc1_supported_standards() : async [SupportedStandard] {
    [
      {
        name = "ICRC-1";
        url = "https://github.com/dfinity/ICRC-1";
      },
    ];
  };

  public query func icrc1_metadata() : async [(Text, Value)] {
    [
      ("icrc1:name", #Text(tokenName)),
      ("icrc1:symbol", #Text(tokenSymbol)),
      ("icrc1:decimals", #Nat(Nat8.toNat(tokenDecimals))),
      ("icrc1:fee", #Nat(transferFee)),
    ];
  };

  public shared ({ caller }) func icrc1_transfer(args : TransferArgs) : async TransferResult {
    let requestedFee = switch (args.fee) {
      case (null) transferFee;
      case (?fee) fee;
    };

    if (requestedFee != transferFee) {
      return #Err(#BadFee({ expected_fee = transferFee }));
    };

    let fromAccount : Account = {
      owner = caller;
      subaccount = args.from_subaccount;
    };

    let fromBalance = getBalance(fromAccount);
    let debitAmount = args.amount + transferFee;

    if (fromBalance < debitAmount) {
      return #Err(#InsufficientFunds({ balance = fromBalance }));
    };

    if (transferFee > 0 and totalSupply < transferFee) {
      return #Err(
        #GenericError({
          error_code = 1;
          message = "Total supply too low for fee burn";
        })
      );
    };

    let toBalance = getBalance(args.to);

    putBalance(fromAccount, fromBalance - debitAmount);
    putBalance(args.to, toBalance + args.amount);

    if (transferFee > 0) {
      totalSupply -= transferFee;
    };

    let txIndex = nextTxIndex;
    nextTxIndex += 1;

    #Ok(txIndex);
  };

  public shared ({ caller }) func mint(to : Account, amount : Nat) : async Bool {
    if (caller != initialOwner) {
      return false;
    };

    if (amount == 0) {
      return false;
    };

    putBalance(to, getBalance(to) + amount);
    totalSupply += amount;
    true;
  };

  // ── Old-ICET redemption ──────────────────────────────────────────────────

  type OldICETActor = actor {
    icrc1_balance_of : query (Account) -> async Nat;
    icrc1_transfer : ({
      from_subaccount : ?Subaccount;
      to : Account;
      amount : Nat;
      fee : ?Nat;
      memo : ?Blob;
      created_at_time : ?Nat64;
    }) -> async TransferResult;
    icrc1_fee : query () -> async Nat;
  };

  transient let OLD_ICET_CANISTER_ID : Text = "ot4zw-oaaaa-aaaag-qabaa-cai";

  // All 0xFF bytes — permanently holds locked old-ICET tokens
  transient let OLD_ICET_BURN_VAULT : Subaccount =
    Blob.fromArray(Array.tabulate<Nat8>(32, func(_) = 0xFF));

  // Derive a 32-byte subaccount from a Principal (principal bytes, zero-padded)
  func principalToSubaccount(p : Principal) : Subaccount {
    let bytes = Blob.toArray(Principal.toBlob(p));
    Blob.fromArray(
      Array.tabulate<Nat8>(32, func(i) {
        if (i < bytes.size()) bytes[i] else 0;
      })
    );
  };

  /// Returns the address where a user must send their old ICET tokens
  /// before calling redeemOldICET().
  public query func getOldICETDepositAddress(user : Principal) : async Account {
    {
      owner = Principal.fromActor(this);
      subaccount = ?principalToSubaccount(user);
    };
  };

  /// Burns old ICET that were deposited to the caller's deposit address and
  /// mints an equivalent amount of new ICET to the caller (1:1, net of the
  /// old-ICET transfer fee).
  ///
  /// Workflow:
  ///   1. Call getOldICETDepositAddress(yourPrincipal) to get your deposit address.
  ///   2. Transfer old ICET to that address on the old canister (ot4zw-oaaaa-aaaag-qabaa-cai).
  ///   3. Call redeemOldICET() — this canister locks the old tokens and mints new ICET.
  public shared ({ caller }) func redeemOldICET() : async TransferResult {
    let callerSubaccount = principalToSubaccount(caller);
    let depositAccount : Account = {
      owner = Principal.fromActor(this);
      subaccount = ?callerSubaccount;
    };

    let oldICET : OldICETActor = actor(OLD_ICET_CANISTER_ID);

    // 1. Check deposited balance on old canister
    let depositBalance = await oldICET.icrc1_balance_of(depositAccount);
    if (depositBalance == 0) {
      return #Err(#InsufficientFunds({ balance = 0 }));
    };

    let oldFee = await oldICET.icrc1_fee();
    if (depositBalance <= oldFee) {
      return #Err(#InsufficientFunds({ balance = depositBalance }));
    };
    // Subtract via Int to avoid the M0155 Nat-underflow trap warning.
    let redeemAmount : Nat = Int.abs((depositBalance : Int) - (oldFee : Int));

    // 2. Move old ICET into the burn vault (locked forever in this canister)
    let vaultAccount : Account = {
      owner = Principal.fromActor(this);
      subaccount = ?OLD_ICET_BURN_VAULT;
    };

    let burnResult = await oldICET.icrc1_transfer({
      from_subaccount = ?callerSubaccount;
      to = vaultAccount;
      amount = redeemAmount;
      fee = ?oldFee;
      memo = null;
      created_at_time = null;
    });

    switch (burnResult) {
      case (#Err(e)) { return #Err(e) };
      case (#Ok(_)) {};
    };

    // 3. Mint new ICET 1:1 to caller's default subaccount
    let toAccount : Account = { owner = caller; subaccount = null };
    putBalance(toAccount, getBalance(toAccount) + redeemAmount);
    totalSupply += redeemAmount;

    let txIndex = nextTxIndex;
    nextTxIndex += 1;
    #Ok(txIndex);
  };
};
