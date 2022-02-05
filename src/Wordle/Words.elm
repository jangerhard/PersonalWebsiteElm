module Wordle.Words exposing (getRandomWord)

import RandomHelpers


getRandomWord : (String -> msg) -> String -> Cmd msg
getRandomWord default =
    RandomHelpers.chooseRandomCommand words default


words : List String
words =
    "abuse\n    adult\n    agent\n    anger\n    apple\n    award\n    basis\n    beach\n    birth\n    block\n    blood\n    board\n    brain\n    bread\n    break\n    brown\n    buyer\n    cause\n    chain\n    chair\n    chest\n    chief\n    child\n    china\n    claim\n    class\n    clock\n    coach\n    coast\n    court\n    cover\n    cream\n    crime\n    cross\n    crowd\n    crown\n    cycle\n    dance\n    death\n    depth\n    doubt\n    draft\n    drama\n    dream\n    dress\n    drink\n    drive\n    earth\n    enemy\n    entry\n    error\n    event\n    faith\n    fault\n    field\n    fight\n    final\n    floor\n    focus\n    force\n    frame\n    frank\n    front\n    fruit\n    glass\n    grant\n    grass\n    green\n    group\n    guide\n    heart\n    henry\n    horse\n    hotel\n    house\n    image\n    index\n    input\n    issue\n    japan\n    jones\n    judge\n    knife\n    laura\n    layer\n    level\n    lewis\n    light\n    limit\n    lunch\n    major\n    march\n    match\n    metal\n    model\n    money\n    month\n    motor\n    mouth\n    music\n    night\n    noise\n    north\n    novel\n    nurse\n    offer\n    order\n    other\n    owner\n    panel\n    paper\n    party\n    peace\n    peter\n    phase\n    phone\n    piece\n    pilot\n    pitch\n    place\n    plane\n    plant\n    plate\n    point\n    pound\n    power\n    press\n    price\n    pride\n    prize\n    proof\n    queen\n    radio\n    range\n    ratio\n    reply\n    right\n    river\n    round\n    route\n    rugby\n    scale\n    scene\n    scope\n    score\n    sense\n    shape\n    share\n    sheep\n    sheet\n    shift\n    shirt\n    shock\n    sight\n    simon\n    skill\n    sleep\n    smile\n    smith\n    smoke\n    sound\n    south\n    space\n    speed\n    spite\n    sport\n    squad\n    staff\n    stage\n    start\n    state\n    steam\n    steel\n    stock\n    stone\n    store\n    study\n    stuff\n    style\n    sugar\n    table\n    taste\n    terry\n    theme\n    thing\n    title\n    total\n    touch\n    tower\n    track\n    trade\n    train\n    trend\n    trial\n    trust\n    truth\n    uncle\n    union\n    unity\n    value\n    video\n    visit\n    voice\n    waste\n    watch\n    water\n    while\n    white\n    whole\n    woman\n    world\n    youth\n    alcon\n    aught\n    hella\n    ought\n    thame\n    there\n    thine\n    thine\n    where\n    which\n    whose\n    whoso\n    yours\n    yours\n    admit\n    adopt\n    agree\n    allow\n    alter\n    apply\n    argue\n    arise\n    avoid\n    begin\n    blame\n    break\n    bring\n    build\n    burst\n    carry\n    catch\n    cause\n    check\n    claim\n    clean\n    clear\n    climb\n    close\n    count\n    cover\n    cross\n    dance\n    doubt\n    drink\n    drive\n    enjoy\n    enter\n    exist\n    fight\n    focus\n    force\n    guess\n    imply\n    issue\n    judge\n    laugh\n    learn\n    leave\n    limit\n    marry\n    match\n    occur\n    offer\n    order\n    phone\n    place\n    point\n    press\n    prove\n    raise\n    reach\n    refer\n    relax\n    serve\n    shall\n    share\n    shift\n    shoot\n    sleep\n    solve\n    sound\n    speak\n    spend\n    split\n    stand\n    start\n    state\n    stick\n    study\n    teach\n    thank\n    think\n    throw\n    touch\n    train\n    treat\n    trust\n    visit\n    voice\n    waste\n    watch\n    worry\n    would\n    write\n    above\n    acute\n    alive\n    alone\n    angry\n    aware\n    awful\n    basic\n    black\n    blind\n    brave\n    brief\n    broad\n    brown\n    cheap\n    chief\n    civil\n    clean\n    clear\n    close\n    crazy\n    daily\n    dirty\n    early\n    empty\n    equal\n    exact\n    extra\n    faint\n    false\n    fifth\n    final\n    first\n    fresh\n    front\n    funny\n    giant\n    grand\n    great\n    green\n    gross\n    happy\n    harsh\n    heavy\n    human\n    ideal\n    inner\n    joint\n    large\n    legal\n    level\n    light\n    local\n    loose\n    lucky\n    magic\n    major\n    minor\n    moral\n    naked\n    nasty\n    naval\n    other\n    outer\n    plain\n    prime\n    prior\n    proud\n    quick\n    quiet\n    rapid\n    ready\n    right\n    roman\n    rough\n    round\n    royal\n    rural\n    sharp\n    sheer\n    short\n    silly\n    sixth\n    small\n    smart\n    solid\n    sorry\n    spare\n    steep\n    still\n    super\n    sweet\n    thick\n    third\n    tight\n    total\n    tough\n    upper\n    upset\n    urban\n    usual\n    vague\n    valid\n    vital\n    white\n    whole\n    wrong\n    young\n"
        |> String.lines
        |> List.map String.trim
