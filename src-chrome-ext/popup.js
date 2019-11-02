getRecentlyClosed = () =>
  new Promise(resolve => chrome.sessions.getRecentlyClosed(resolve))

async function main() {
  const sessions = await getRecentlyClosed()
  console.log('sessions', sessions)

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
}

function logTab({ title, url }) {
  console.log('tab', { title, url })
}
main().catch(console.error)
