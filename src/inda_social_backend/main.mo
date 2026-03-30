import User "./modules/User";

shared ({caller = DEPLOYER}) persistent actor class() = this  {

  let userDB = User.init(DEPLOYER);

  public shared ({ caller }) func signUp(dataInit: User.UserDataInit ): async User.SignUpResponse{
    User.signUp(userDB, caller, dataInit)
  };

  public shared query ({ caller }) func login(): async User.LoginResponse {
    User.login(userDB, caller)
  };

  public shared ({ caller }) func editProfile(data: User.EditableData): async User.SignUpResponse {
    User.updateDateProfile(userDB, caller, data)
  };

  public shared ({ caller }) func requestCreatorProfile(dataInit: User.CreatorDataInit): async {#Ok: Int; #Err: Text} {
    User.requestCreatorProfile(userDB, caller, dataInit)
  };


  //--------------------- Admin functions ---------------------//

  public shared ({ caller }) func addAdmin(p: Principal): async { #Ok; #Err } {
    User.addAdmin(userDB, caller, p)
  };

  public shared query ({ caller }) func getRequestRole(): async {#Ok: [(Principal, User.Request)]; #Err: Text}  {
    User.getAllRequests(userDB, caller)
  };

  public shared query ({ caller }) func isAdmin(p: ?Principal): async Bool {
    let target = switch p {
      case null caller;
      case (?p) p
    };
    User.isAdmin(userDB, target)
  };


};
