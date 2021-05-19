import scala.continuations.*
import scala.concurrent.Future

def projectsFor(user: User): Future[List[Project]] =
  for
    public <- getPublicProjects(user)
    isPro <- hasProAccount(user)
    private <- if isPro then
        getPrivateProjects(user)
      else
        Future(Nil)
  yield public ++ private

// At this point we can just rename `extract` to `await`
def projectsFor2(user: User) =
  WithFutures[List[Project]].run {
    val public = getPublicProjects(user).extract
    if hasProAccount(user).extract then
      public ++ getPrivateProjects(user).extract
    else public
  }