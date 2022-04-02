

-- A few different ideas
-- So, basically, I need to combine mainServer with mainClient. Both are Socket -> IO ()

combine :: Socket -> IO ()
combine = with s $ forever $ do
   res1 <- clientSend "my awesome message"
   res2 <- serverSend (if res1=="my awesome message" then "very cool" else "not cool")
   res3 <- clientSend (if res2=="very cool" then "thanks!" else "sad")
   return ()

combine2 :: Socket -> IO ()
combine2 = with s $ forever $ do
   clientState <- clientState get
   res1 <- clientSend clientState
   if res1 
       then serverSend "messageOne"
       else serverSend "messageTwo"

-- lets go crazy
combine3 :: Socket -> IO ()
combine3 s = with s $ do
    cs <- clientDo$ get
    res1 <- serverDo$ send (10*121)
    clientDo$ put res1
    res2 <- clientDo$ send "now way!!"
    res3 <- serverDo$ computationallyIntensiveFuncion res2
    clientDo$ put res3 

-- Another idea
combine4 :: Socket -> IO ()
combine4 s = with s $ do
    s1 <- nDo 1 $ get
    s2 <- nDo 2 $ send (10*121)
    nDo 

-- This syntax is okay. Maybe template haskell could clean it up though
combine5 :: Socket -> IO ()
combine5 s = with s $ do
    hdo id1$ put 3
    hdo id2$ put 2
    s <- hdo id3$ get
    res <- hdo id1$ performTask s
    hdo id3$ put res
    hdo id3$ put res
    case res of
        ("val 1", "val3") -> hdo id1$ put 4
        (_, "nice") -> hdo id2$ log "got it"
        _ -> hdo id3$ log "got it here"
    res2 <- hdo id2$ expensiveFunction res
    return undefined
    where id1 = "my client"
          id2 = "my ultra-remote-backup"
          id3 = "low latency cache"

-- dynamically run via flag: runTask "my client" combine5 socket 

-- Okay, cool idea. Can we do lists of clients and become even more general?
-- Can we 
combine6 :: IO ()
combine6 = do
    hdo[id1,id2]$ put 3
    [one,two] <- hdo[id1,id2]$ get
    res <- hdo id4$ expensive $ get
    if | expensive1P res -> hdo id1$ put res
       | expensive2P res -> hdo id2$ put res
       | expensive3P res -> hdo id3$ put res
       | _               -> hdo id4$ put res
    

-- Expensive operation
combine7 :: IO ()
combine7 = do
    hdo[id1,id2]$ put 3
    [one,two] <- hdo[id1,id2]$ get
    (did, res) <- hdo id4$ (\v -> (comp v, v)) . expensive <$> get  -- The did (dynamic id) must be communicated 
    hdo did$ put res
    where comp res = if | expensive1P res -> id1
                        | expensive2P res -> id2
                        | expensive3P res -> id3
                        | _               -> id4

-- Large operation
combine7 :: IO ()
combine7 = do
    hdo[id1,id2]$ put 3
    [one,two] <- hdo[id1,id2]$ get
    res <- hdo id4$ expensive <$> get
    if | expensive1P res -> hdo id1$ put res
       | expensive2P res -> hdo id2$ put res
       | expensive3P res -> hdo id3$ put res
       | _               -> hdo id4$ put res

-- Group ops (we go into a different kind of group)
combine8 :: IO ()
combine8 = do
    hdo group1 $ put 8
    hdo 

combine9 :: IO ()
combine9 = do
    (one, two) <- (,) <$> hdo id1 get <*> hdo id2 get
    (one', two') <- liftA2 (,) (hdo id1 get) (hdo id2 get)
    [one'', two''] <- traverse (\id -> hdo id get) [id1, id2]

-- our container is perhaps just a functor! Or maybe a foldable, or maybe a traversable?
combine10 :: IO ()
combine10 = do
    traverse (\id -> hdo id put 4) [id1, id2, id3]
    where
        group :: Functor f => f HandshakeID


-- Lets go simple, and model multiple clients (of x >= 1), and a single server
-- To run multiple clients, a single client would connect with stack run -- -c, then another could
-- with stack run -- -c, then another could with stack run -- -c... The next client would hop in
-- at the start of the execution of the function? Is it possible to start before that?
combine11 :: IO ()
combine11 = do
    res <- hdo s$ expensiveComp <$> get
    hdof cs$ put res
    res <- hdof cs$ get     -- This goes into a functor right?? Or can the thing change? Actually, res :: f a
    res <- hdo s$ put expensiveComp' res
    -- How can I do a computation per client? Clients won't know of each other? Or do we want clients to know of eachother?
    res <- hdof cs$ get
    let toGoToClients = expensiveComp'' <$> res -- Each client got their own expensiveComp'' done!
    -- I could go two ways. The clients are locked in sync, or async. Probably best async. Then I would write per client I think
    -- How would the server know to deal with the extra clients?    


    where cs :: Functor f => f HandshakeID
          s :: HandshakeID

-- I think we should have other things, for example. How should we do this? Option one, thread them
-- Simplisity is that the relations remain s-c
-- stack run -- -s : starts the server
-- stack run -- -c : start thread for s-c1
-- stack run -- -c : start thread for s-c2
-- stack run -- -c : start thread for s-c3

-- Option two. Lock the s-cs
-- stack run -- -s : starts the server
-- stack run -- -c : start relationship for s-[c1]
-- stack run -- -c : Wait for s-[c1] to end, then do s-[c1,c2]
-- stack run -- -c : Wait for s-[c1,c2] to end, then do s-[c1,c2,c3]

-- Is there an option inbetween. Is there a way we can start the s-newc but not wait for previous
-- to sync back up. Do we really want things locking up?

-- I think option to is special, and we can keep at as such. Laziness might be the key to running it
-- Really, this project is about simulating option two. Yeah, we should be able to go along, even if
-- One of the client is unresponsive. But s will expect the client to enter at one point. But the function
-- should start at one point.
combine12 :: IO ()
combine12 = do
    res <- hdos cs$ get  -- Here res is f a
    -- I could do something like this. Expect everything to be functory. Or it could be applicaitve? 
    -- Our functor, does it contain a label for each thing? Maybe an id for each container? Functor (Reader ID) a
    -- (<*>) :: f (a -> b) -> f a -> f b
    hdos cs$ 
    hdos cs$ put $ expensiveComp <$> res
    
-- Hmm, I don't see a way to kindly deal with multiple unknown clients. The best way is to probably
-- do multiple threads of a single one


-- Plan, Simplist to more complicated topoology
-- 1. Do single c-s using the current thing
-- 2. Do multiple clients, c1-c2-c3-... Like for long term, cache, high computation, high GPU
-- 3. Do one or many clients (is this possible or sensible?)




testin :: IO ()
testin = do
    do putStrLn "hello"
    do putStrLn "hello world"
    do putStrLn "Another"


-- Are applicatives or functors possible?
-- what other instances?



-- combined :: (Client (Constraint1, Constraint2, Constraint3), Server )