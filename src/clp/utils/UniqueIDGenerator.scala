package clp.utils

class UniqueIDGenerator {
  private var lastID: Int = 0
  private var returnedIDs: List[Int] = Nil
  
  def get = if(returnedIDs.isEmpty) {
    val id = lastID
    lastID = lastID + 1
    id
  } else {
    val id = returnedIDs.head
    returnedIDs = returnedIDs.tail
    id
  }
  
  def returnID(id: Int) = returnedIDs = id :: returnedIDs
}