import Map "mo:map/Map";
import Set "mo:map/Set";
import { phash } "mo:map/Map";
import Principal "mo:core/Principal";
import { now } "mo:core/Time";
import Int "mo:core/Int";
import Array "mo:core/Array";
import Shared "../../Shared";

module {

  // ===============================================
  // 1. Tipos de Utilidad / Base (Base Types)
  // ===============================================

  public type MetadataPart = Shared.MetadataPart;
  public type Metadata = Shared.Metadata;
  public type Value = Shared.Value;

  public type Verification = {
    #Email;
    #Phone;
    #Custom : MetadataPart;
  };

  public type GovernmentID = {
    docType: Text;
    value: Text;
  };

  public type LegalStatusBrand = {
    #Registered;
    #Unregistered;
    #Pending;
    #Other : Text;
  };

  public type Industry = {
    #Fashion;
    #Sustainable;
    #Inclusion;
    #Innovation;
    #Other : Text;
  };

  public type EditableData = {
    firstName : Text;
    lastName: Text;
    email : ?Text;
    bio : Text;
    avatar : ?Blob;
    thumbnail : ?Blob;
    metadata : Metadata;
  };

  public type Request = {
    metadata: Metadata;
    id: Int;
    kind: {
      #NewCreator: CreatorDataInit;
      #NewBrand: {};
      #NewPartnership: {}
    }
  };

  // ===============================================
  // 2. Tipos de Entidad Principal (Main Entity Types)
  // ===============================================

  public type User = EditableData and {
    principal : Principal;
    scoring : Nat;
    lastActivity : Int;
    roleRequestedOrAsigned: Bool; 
    verifications : [Verification];
  };

  public type UserPreview = {
    thumbnail : ?Blob;
    scoring: Nat;
    principal: Principal;
    name: Text; // Se puede extraer del usuario base o si tiene rol se extrae del Rol
  };

  public type Creator = {
    verified : Bool;
    governmentID : [GovernmentID];
    webSite : [Text];
    portfolio : [Text];
    extendedData : Metadata;
  };

  public type Brand = {
    verified : Bool;
    governmentID : [GovernmentID];
    brandName : Text;
    industry : Industry;
    availableCountries : [Text];
    webSite : Text;
    socialMedia : [Text];
    extendedData : Metadata;
  };

  public type Partnership = {
    verified: Bool;
    status: {#registered; #unregistered};
    socialMedia: [Text];
    industry: Industry;
    webSite: Text;
    availableCountries: [Text];
    extendedData : Metadata;
  };

  // ===============================================
  // 3. Tipos de Request/Response
  // ===============================================

  public type UserDataInit = {
    firstName : Text;
    lastName : Text;
    email: ?Text;
    bio: Text;
  };

  public type CreatorDataInit = {
    governmentID : [GovernmentID];
    webSite : [Text];
    portfolio : [Text];
    extendedData : Metadata;
  };

  public type SignUpResponse = {  #Ok : User; #Err : {#User: User; #Msg: Text}  };
  public type LoginResponse = { 
    #Ok : {
      user: User; 
      creator: ?Creator; 
      brand: ?Brand; 
      partner: ?Partnership;
    }; 
    #Err : Text 
  };

  // ===============================================
  // 4. Tipo de Estado (State Type)
  // ===============================================

  public type State = {
    users : Map.Map<Principal, User>;
    requests: Map.Map<Principal, Request>;
    creators : Map.Map<Principal, Creator>;
    brands : Map.Map<Principal, Brand>;
    partnerships : Map.Map<Principal, Partnership>;
    admins : Set.Set<Principal>;
  };

  // ===============================================
  // 5. Funciones Privadas
  // ===============================================


  func newUser(dataInit: UserDataInit, caller: Principal): User {
    {
      dataInit with
      avatar = null;
      metadata = [];
      principal = caller;
      thumbnail = null;
      lastActivity = now();
      roleRequestedOrAsigned = false;
      scoring : Nat = 0;
      verifications = [];
    }
  };

  // func newCreator(dataInit: CreatorDataInit, caller: Principal): Creator {
  //   { dataInit with  verified : Bool = false }
  // };

  func pushRequest(s: State, r: Request, u: Principal) { 
    ignore Map.put(s.requests, phash, u, r)
  };

  func resolveUserName(s: State, u: User): Text {
    let creator = Map.get(s.creators, phash, u.principal);
    let brand = Map.get(s.brands, phash, u.principal);
    let partnership = Map.get(s.partnerships, phash, u.principal);
    let nameRoleProfile = switch (creator, brand, partnership) {
      case (?c, null, null) {
        let nameItem = Array.find<MetadataPart>(
          c.extendedData, 
          func item = switch (item.key) {
            case ("ArtisticName") true;
            case (_) false 
          }
        );
        switch nameItem {
          case (?nameItem) {
            switch(nameItem.value) { case(#Text(name)) name; case(_) "" }
          };
          case _ ""
        }
      };
      case (null, ?b, null) {
        let nameItem = Array.find<MetadataPart>(
          b.extendedData, 
          func item = switch (item.key) {
            case ("BrandName") true;
            case (_) false 
          }
        );
        switch nameItem {
          case (?nameItem) {
            switch(nameItem.value) { case(#Text(name)) name; case(_) "" }
          };
          case _ ""
        }
      };
      case (null, null, ?p) {
        let nameItem = Array.find<MetadataPart>(
          p.extendedData, 
          func item = switch (item.key) {
            case ("PartenerName") true;
            case (_) false 
          }
        );
        switch nameItem {
          case (?nameItem) {
            switch(nameItem.value) { case(#Text(name)) name; case(_) "" }
          };
          case _ ""
        }
      };
      case (_, _, _ ) {
        ""
      }
    };
    if (nameRoleProfile != "") nameRoleProfile else u.firstName # " " # u.lastName
  };
  // ===============================================
  // 6. Funciones Publicas
  // ===============================================

  public func init(admin: Principal) : State {
    {
      users = Map.new<Principal, User>();
      requests = Map.new<Principal, Request>();
      creators = Map.new<Principal, Creator>();
      brands = Map.new<Principal, Brand>();
      partnerships = Map.new<Principal, Partnership>();
      admins = Set.make<Principal>(phash, admin);
      // admins = Set.new<Principal>();
    };
  };

  public func signUp(s : State, caller : Principal, inputData: UserDataInit) : SignUpResponse {
    if(Principal.isAnonymous(caller)){
      return #Err(#Msg("Anonymous identity")) 
    };
    switch (Map.get<Principal, User>(s.users, phash, caller)) {
      case null {
        let user = newUser(inputData, caller);
        ignore Map.put<Principal, User>(s.users, phash, caller, user);
        #Ok(user);
      };
      case (?user) { #Err(#User(user)) };
    };
  };

  public func updateDateProfile(s: State, caller : Principal, inputData: EditableData) : SignUpResponse {
    switch (Map.get<Principal, User>(s.users, phash, caller)) {
      case null return #Err(#Msg("User not found"));
      case (?user) {
        let isVerificatedEmail = Array.find<Verification>(user.verifications, func v = v == #Email) != null;
        let verified = isVerificatedEmail and (user.email != inputData.email);
        let {avatar; bio; email; firstName; lastName; metadata; thumbnail} = inputData;
        let updatedUser = {
          user with 
          avatar;
          bio;
          email;
          firstName;
          lastName;
          metadata;
          thumbnail;
          verified;
        };
        ignore Map.put(s.users, phash, caller, updatedUser);
        #Ok(updatedUser)
      };
    };
  };

  public func requestCreatorProfile(s: State, caller: Principal, dataInit: CreatorDataInit): {#Ok: Int; #Err: Text} {
    switch (getUser(s, caller)) {
      case null return #Err("UserNotFound");
      case ( ?user ) {
        if(user.roleRequestedOrAsigned) {
          return #Err("The user already has a role assigned or requested")
        } else {
          let id = now();
          let newRequest: Request = {
            metadata = [];
            id ;
            kind = #NewCreator(dataInit)
          };
          ignore Map.put(s.users, phash, caller, {user with roleRequestedOrAsigned = true});
          pushRequest(s, newRequest, caller);
          #Ok(id)
        }
      }
    };
  };

  public func login(s : State, caller : Principal) : LoginResponse {
    if(Principal.isAnonymous(caller)){
      return #Err("Anonymous identity") 
    };
    switch (Map.get<Principal, User>(s.users, phash, caller)) {
      case (?user) {
        #Ok(
          {
            user;
            creator = Map.get<Principal, Creator>(s.creators, phash, caller);
            brand = Map.get<Principal, Brand>(s.brands, phash, caller);
            partner = Map.get<Principal, Partnership>(s.partnerships, phash, caller);
          }
        );
      };
      case null { #Err("UserNotFound") };
    };
  };


  public func isAdmin(s : State, caller : Principal) : Bool {
    Set.has<Principal>(s.admins, phash, caller);
  };

  public func getUser(s : State, p : Principal) : ?User {
    Map.get<Principal, User>(s.users, phash, p);
  };

  public func getAllUsers(s: State) : [User] {
    Array.map<(Principal, User), User>(
      Map.toArray(s.users), 
      func entry = {entry.1 with thumbnail = null; avatar = null}
    )
  };

  public func isUser(s : State, p : Principal) : Bool {
    Map.has<Principal, User>(s.users, phash, p);
  };


  public func getUserPreview(s: State, p: Principal): {#Ok: UserPreview; #Err: Text } {
    switch(getUser(s, p)) {
      case null #Err("User not found");
      case ( ?user ) {
        #Ok({
          user with
          name = resolveUserName(s, user)
        })
      }
    }
  };

  public func isCreator(s: State, p: Principal): Bool {
    Map.has<Principal, Creator>(s.creators, phash, p);
  };

  public func isBrand(s: State, p: Principal): Bool {
    Map.has<Principal, Brand>(s.brands, phash, p);
  };

  public func isPartnership(s: State, p: Principal): Bool {
    Map.has<Principal, Partnership>(s.partnerships, phash, p);
  };

  /// Admin functions 
  public func getAllRequests(s: State, caller: Principal): {#Ok: [(Principal, Request)]; #Err: Text} {
    if(not isAdmin(s, caller)) { return #Err("Access denied")};
    #Ok((Map.toArray(s.requests)));
  };


  public func addAdmin(s: State, caller: Principal, newAdmin: Principal): { #Ok; #Err } {
    if(not isAdmin(s, caller)) { return #Err};
    ignore Set.put(s.admins, phash, newAdmin);
    #Ok
  };

  public func acceptRoleRequest(s: State, caller: Principal, user: Principal) : { #Ok; #Err } {
    if(not isAdmin(s, caller)) { return #Err};
    switch(Map.get<Principal, Request>(s.requests, phash, user)){
      case null return #Err;
      case (?r) {
        
        switch (r.kind){
          case (#NewCreator(data)) {
            let newCreator: Creator = {data with verified = true};
            let dataRegistry: Metadata = [
              {key = "requestRegistryDate"; value = #Int(r.id)},
              {key = "acceptRegistryDate"; value = #Int(now())},
              {key = "acceptorAdmin"; value = #Text(Principal.toText(caller))}
            ];
            let extendedData = Array.concat(data.extendedData, dataRegistry);
            ignore Map.put(s.creators, phash, user, {newCreator with extendedData});

            // Conservar la fecha de registro
            ignore Map.remove(s.requests, phash, user);
          };
          case _ { } //TODO
        }
      };
    };
    return #Ok
  };
  
  public func rejectRoleRequest(s: State, caller: Principal, user: Principal) : { #Ok; #Err } {
    if(not isAdmin(s, caller)) { return #Err};
    ignore Map.remove <Principal, Request>(s.requests, phash, user);  
    return #Ok
  } 

  // ===============================================
  // 6. Funciones Publicas
  // ===============================================
};
