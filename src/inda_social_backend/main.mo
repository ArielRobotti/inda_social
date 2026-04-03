import User "./modules/User";
import Blog "./modules/Blog";

shared ({caller = DEPLOYER}) persistent actor class() = this  {

  let userDB = User.init(DEPLOYER);
  let blog = Blog.init();

  //----------- User funcionalities ----------//

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

  public shared ({ caller }) func acceptRoleRequest(requester: Principal): async {#Ok; #Err } {
    switch(User.acceptRoleRequest(userDB, caller, requester)){
      case (#Err) #Err;
      case ( #Ok ) {
        #Ok(Blog.newPublisher(blog, caller))
      }
    }
  };

  public shared ({ caller }) func rejectRoleRequest(requester: Principal): async {#Ok; #Err } {
    User.rejectRoleRequest(userDB, caller, requester)
  };

  public shared query ({ caller }) func isAdmin(p: ?Principal): async Bool {
    let target = switch p {
      case null caller;
      case (?p) p
    };
    User.isAdmin(userDB, target)
  };

  public shared query ({ caller }) func getUser(p: Principal): async {#Ok: User.User; #Err} {
    if(not User.isAdmin(userDB, caller)) return #Err;
    switch (User.getUser(userDB, p)) {
      case null #Err;
      case (?u) #Ok(u)
    };
  };

  public shared ({ caller }) func getAllUsers(): async {#Ok: [User.User]; #Err: Text} {
    if(not User.isAdmin(userDB, caller)) return #Err("Caller is not admin");
    #Ok(User.getAllUsers(userDB));
  };

  //--------------------- Blog section -----------------------//
  public shared ({ caller }) func createPost(data: Blog.PostDataInit): async {#Ok: Blog.PostPreview; #Err: Text} {
    let author = User.getUserPreview(userDB, caller);
    switch author {
      case (#Err(e)) #Err(e);
      case (#Ok(author)) {
        Blog.createPost(blog, author, data)
      }
    }
  };

  public shared ({ caller }) func editPost({postId: Int; updatedData: Blog.PostEditableData}): async {#Ok: Blog.PostPreview; #Err: Text}{
    Blog.editPost(blog, caller, postId, updatedData);
  };

  public shared query ({ caller }) func readPost(postId: Int): async {#Ok: Blog.PostExpand; #Err: Text} {
    Blog.readPost(blog, caller, postId);
  };
  
  public shared ({ caller }) func getPaginatePost({page: Nat}): async {posts: [Blog.PostPreview]; hasNext: Bool}{
    Blog.getPaginatePosts(blog, caller, page)
  };

  public shared ({ caller }) func deletePost(postId: Int): async {#Ok; #Err: Text} {
    Blog.deletePost(blog, caller, postId);
  };

  


};
