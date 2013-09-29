module String = struct
  include String include BatString
  end
module Buffer = struct
  include BatBuffer let (+>) buf chr = Buffer.add_char buf chr; buf
  let (+>>) buf str = Buffer.add_string buf str; buf
  end
