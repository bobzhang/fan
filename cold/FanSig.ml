open LibUtil
type ('a,'loc) stream_filter = ('a* 'loc) XStream.t -> ('a* 'loc) XStream.t 