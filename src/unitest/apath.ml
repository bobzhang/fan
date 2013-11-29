module X = struct
  module Y = struct
    module Make (S:U) = struct
      let v = __MODULE__ ^ __BIND__
    end
  end

  module Z = struct
    module Make (S:U) = struct
      let v = __MODULE__  ^ __BIND__
    end
  end 
      
end
let v = __MODULE__ ^ __BIND__
