getRecentlyClosed = () =>
  new Promise(resolve => chrome.sessions.getRecentlyClosed(resolve))

async function main() {
  const sessions = await getRecentlyClosed()
  console.debug('sessions', sessions)

  sessions.forEach(session => {
    const tab = session.tab
    const window = session.window
    if (tab) {
      logTab(tab)
    }
    if (window) {
      // console.log("window.tabs", window.tabs)
      window.tabs.forEach(logTab)
    }
  })

  chrome.history.search(
    { startTime: Date.now() - days(1), text: 'Netflix' },
    arr => arr.forEach(it => logHistoryItem(it)),
  )
}

function days(count) {
  return 1000 * 60 * 60 * 24 * count
}

function logTab({ title, url }) {
  console.debug('tab', { title, url })
}

function logHistoryItem({ title, url, lastVisitTime }) {
  console.log(new Date(lastVisitTime), title, url)
}

main().catch(console.error)
