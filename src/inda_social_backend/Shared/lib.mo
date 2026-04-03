// import User "../User";

module {
  public type Value = {
    #Nat : Nat;
    #Int : Int;
    #Blob : Blob;
    #Text : Text;
    #Array : [Value];
    #Map : [(Text, Value)];
  };

  public type MetadataPart = {
    key : Text;
    value : Value;
  };

  public type Metadata = [MetadataPart];
};
