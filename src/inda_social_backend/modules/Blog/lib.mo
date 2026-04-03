import Map "mo:map/Map";
import Set "mo:map/Set";
import { phash; ihash } "mo:map/Map";
import User "../User";
import Shared "../../Shared";
import { now } "mo:core/Time";

module {

  public type Blog = {
    publishers: Set.Set<Principal>;
    vipReaders: Set.Set<Principal>;
    posts : Map.Map<Int, Post>;
    sensuredPosts : Map.Map<Int, Post>;
  };

  public type PostDataInit = {
    title : Text;
    description : Text;
    image : Blob;
    thumbnail : Blob;
    access : Access;
    extraData : Shared.Metadata;
  };

  public type PostEditableData = {
    title : Text;
    description : Text;
    access : Access;
    extraData : Shared.Metadata;
  };

  public type Access = {
    #Private;
    #Vip;
    // #Regulated : (Principal) -> Bool;
    #Public;
  };

  public type Post = PostDataInit and {
    author: User.UserPreview;
    date : Int;
    lastModification: Int;
    likes : [Principal];
    dislikes : [Principal];
  };

  public type PostPreview = {
    date : Int;
    author : Text; // Se extrae del usario asociado al owner
    title : Text;
    description : Text;
    thumbnail : Blob;
    likes : Nat;
    dislikes : Nat;
    access : Access;
  };

  public type PostExpand =  {
    date : Int;
    title : Text;
    description : Text;
    thumbnail : Blob;
    likes : Nat;
    dislikes : Nat;
    image : Blob;
    access : Access;
    extraData : Shared.Metadata;
    author: User.UserPreview;
  };

  //------------------- Private functions --------------------//

  func postPreview (post: Post): PostPreview {
    {
      post with
      likes = post.likes.size();
      dislikes = post.dislikes.size();
      author = post.author.name
    }
  };

  func postExpand(post: Post): PostExpand {
    {
      post with
      likes = post.likes.size();
      dislikes = post.dislikes.size();
      author = post.author
    }
  };

  //------------------- initialice funciotns -----------------//
  public func init(): Blog {
    {
      posts = Map.new<Int, Post>();
      publishers = Set.new<Principal>();
      sensuredPosts = Map.new<Int, Post>();
      vipReaders = Set.new<Principal>();
    }
  };

  //----------- permision management section -----------------//

  public func newPublisher(b: Blog, p: Principal): () {
    ignore Set.put(b.publishers, phash, p)
  };

  public func removePublisher(b: Blog, p: Principal): () {
    ignore Set.remove(b.publishers, phash, p)
  };

  public func newVipReader(b: Blog, p: Principal): () {
    ignore Set.put(b.vipReaders, phash, p)
  };

  public func removeVipReader(b: Blog, p: Principal): () {
    ignore Set.remove(b.vipReaders, phash, p)
  };


  //------------------- Public functions ---------------------//

  public func createPost(b: Blog, author: User.UserPreview, postDataInit: PostDataInit) : {#Ok: PostPreview; #Err: Text} {
    if(not Set.has<Principal>(b.publishers, phash, author.principal)) {
      return #Err("Caller cant publish")
    };
    let date = now(); // Date tambien funciona como ID
    let newPost: Post = {
      postDataInit with
      author;
      date = date;
      lastModification = date;
      likes = [];
      dislikes = [];
    };
    ignore Map.put(b.posts, ihash, date, newPost );
    #Ok(postPreview(newPost))
  };

  public func editPost(b: Blog, caller: Principal, postId: Int, updatedData: PostEditableData): {#Ok: PostPreview; #Err: Text} {
    if(not Set.has<Principal>(b.publishers, phash, caller)) {
      return #Err("Caller cant publish or edit previous post")
    };
    let post = switch (Map.get(b.posts, ihash, postId)){
      case null return #Err("post not found");
      case (?post) {
        if ( caller != post.author.principal) {
          return #Err("Caller is not post author");
        } else 
        {
          post
        }
      };
    };
    let updatedPost: Post = {
      post with
      lastModification = now();
      title = updatedData.title;
      description = updatedData.description;
      access = updatedData.access;
      extraData = updatedData.extraData;
    };
    ignore Map.put(b.posts, ihash, postId, updatedPost);
    #Ok(postPreview(updatedPost))

  };

  public func readPost(b: Blog, caller: Principal, postId: Int): {#Ok: PostExpand; #Err: Text} {
    let post = switch(Map.get(b.posts, ihash, postId)){
      case null return #Err("Post not found");
      case ( ?post ) post
    };
    if(caller == post.author.principal) {
      return #Ok(postExpand(post))
    };
    switch (post.access) {
      case (#Private) return #Err("Only owner access");
      case ( #Vip ) {
        if(not Set.has(b.vipReaders, phash, caller)){
          return #Err("Only Vip members access")
        };
      };
      // case (#Regulated(f)) {
      //   if(not (f(caller))) {
      //     return #Err("Not match caller/access policies")
      //   };
      // };
      case _ {}
    };
    #Ok(postExpand(post))
  };

  public func deletePost(b: Blog, caller: Principal, postId: Int): {#Ok; #Err: Text} {
    let post = switch(Map.get(b.posts, ihash, postId)){
      case null return #Err("Post not found");
      case ( ?post ) post
    };
    if(caller == post.author.principal) {
      ignore Map.remove(b.posts, ihash, postId);
      return #Ok
    } else {
      return #Err("Caller is not owner")
    }
  };

  public func sensurePost(b: Blog, postId: Int): {#Ok; #Err: Text} {
    let sensuredPost = Map.remove(b.posts, ihash, postId);
    switch sensuredPost {
      case null return #Err("Post not found");
      case ( ?post ) {
        ignore Map.put(b.sensuredPosts, ihash, postId, post);
        #Ok
      }
    }
  };

  public func unSensurePost(b: Blog, postId: Int): {#Ok; #Err: Text} {
    let unSensuredPost = Map.remove(b.sensuredPosts, ihash, postId);
    switch unSensuredPost {
      case null return #Err("Post not found");
      case ( ?post ) {
        ignore Map.put(b.posts, ihash, postId, post);
        #Ok
      }
    }
  };


};
