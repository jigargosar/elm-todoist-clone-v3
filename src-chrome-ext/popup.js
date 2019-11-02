getRecentlyClosed = () =>
  new Promise(resolve => chrome.sessions.getRecentelyClosed(resolve))

async function main() {
  const sessions = await getRecentlyClosed()
  console.log('sessions', sessions)
}

main().catch(console.error)
