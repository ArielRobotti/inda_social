import User "./modules/User";

persistent actor {

  let userDB = User.init();

  public shared ({ caller }) func signUp(dataInit: User.UserDataInit ): async User.SignUpResponse{
    User.signUp(userDB, caller, dataInit)
  };

  public shared ({ caller }) func login(): async User.LoginResponse {
    User.login(userDB, caller)
  };

  public shared ({ caller }) func requestCreatorProfile(dataInit: User.CreatorDataInit): async {#Ok: Int; #Err: Text} {
    User.requestCreatorProfile(userDB, caller, dataInit)
  };
};
