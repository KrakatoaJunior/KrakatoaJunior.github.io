// year-video-slider.js
class YearVideoSlider extends HTMLElement {
  static get observedAttributes() { return ['src','start','end']; }

  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
    this._years = [];
    this._isScrubbing = false;
    this._autoplayOn = true;
  }

  attributeChangedCallback() { this._render(); }
  connectedCallback() { this._render(); }

  _render() {
    const src   = this.getAttribute('src')   || '';
    const start = parseInt(this.getAttribute('start') || '2005', 10);
    const end   = parseInt(this.getAttribute('end')   || '2023', 10);
    if (!Number.isFinite(start) || !Number.isFinite(end) || end < start) return;

    this._years = Array.from({length: end - start + 1}, (_,i)=> start + i);

    this.shadowRoot.innerHTML = `
      <style>
        :host{display:block;max-width:1000px}
        .wrap{padding:12px}
        .video-frame{width:100%;aspect-ratio:3/2;background:#000;border-radius:12px;overflow:hidden;box-shadow:0 8px 24px rgba(0,0,0,.15)}
        video{width:100%;height:100%;object-fit:contain;display:block;background:#000}

        .controls{display:grid;grid-template-columns:auto 1fr;gap:12px;align-items:center;margin-top:14px}

        /* Play/Pause button */
        .toggle{
          display:inline-flex;align-items:center;justify-content:center;
          width:44px;height:44px;border-radius:999px;border:1px solid #e5e7eb;
          background:#fff;cursor:pointer;transition:transform .12s ease, box-shadow .12s ease;
        }
        .toggle:hover{ transform:scale(1.06); box-shadow:0 6px 14px rgba(0,0,0,.12); }
        .toggle:active{ transform:scale(.96) }
        .toggle svg{width:22px;height:22px;fill:#111827}

        /* Timeline with CSS custom props so we can "pop" independently */
        .timeline{
          position:relative;height:52px;display:flex;align-items:center;
          --track-h: 4px; --thumb-d: 16px; --accent:#2563eb; --track:#e5e7eb;
          transition: filter .12s ease, transform .12s ease;
        }
        /* HOVER: track only */
        .timeline.hover-track{ --track-h: 6px; filter: drop-shadow(0 1px 2px rgba(0,0,0,.15)); }
        /* HOVER: thumb only */
        .timeline.hover-thumb{ --thumb-d: 20px; }
        /* SCRUBBING: stronger but separate state */
        .timeline.scrubbing{ --track-h: 6px; --thumb-d: 18px; filter: drop-shadow(0 1px 2px rgba(0,0,0,.18)); }

        .timeline input[type="range"]{
          -webkit-appearance:none;appearance:none;width:100%;height:var(--track-h);
          background: var(--track); border-radius:999px; outline:none; margin:0;
          transition: background-size 0.06s linear, height .12s ease;
          cursor: pointer;
        }
        .timeline input[type="range"]::-webkit-slider-thumb{
          -webkit-appearance:none;appearance:none;width:var(--thumb-d);height:var(--thumb-d);
          border-radius:50%; background:var(--accent); border:0; cursor:pointer;
          box-shadow:0 0 0 4px rgba(37,99,235,.15); transition: width .12s ease, height .12s ease, box-shadow .12s ease;
        }
        .timeline input[type="range"]::-moz-range-thumb{
          width:var(--thumb-d);height:var(--thumb-d);border:0;border-radius:50%;
          background:var(--accent);cursor:pointer;box-shadow:0 0 0 4px rgba(37,99,235,.15);
          transition: width .12s ease, height .12s ease, box-shadow .12s ease;
        }
        .timeline.hover-thumb input[type="range"]::-webkit-slider-thumb{ box-shadow:0 0 0 6px rgba(37,99,235,.20); }
        .timeline.hover-thumb input[type="range"]::-moz-range-thumb{ box-shadow:0 0 0 6px rgba(37,99,235,.20); }

        .timeline input[type="range"]::-webkit-slider-runnable-track{
          height:var(--track-h); border-radius:999px;
          background:linear-gradient(to right, var(--accent) var(--progress,0%), var(--track) 0);
          transition: height .12s ease;
        }
        .timeline input[type="range"]::-moz-range-track{
          height:var(--track-h); border-radius:999px; background:#e5e7eb;
          transition: height .12s ease;
        }

        /* Decorative pips: centered vertically, non-interactive */
        .pips{position:absolute; inset:0; pointer-events:none;}
        .pip{position:absolute; top:50%; transform:translate(-50%,-50%); width:0; height:var(--track-h);}
        .pip::before{
          content:""; position:absolute; left:50%; top:50%; transform:translate(-50%,-50%);
          width:2px; height:var(--track-h); background:#9ca3af; border-radius:1px;
        }

        .sr-only{position:absolute;width:1px;height:1px;padding:0;margin:-1px;overflow:hidden;
                 clip:rect(0,0,0,0);white-space:nowrap;border:0}
      </style>

      <div class="wrap">
        <div class="video-frame">
          <video id="vid" src="${src}" muted autoplay playsinline preload="auto" aria-label="Animated timeline"></video>
        </div>
        <div class="controls" role="group" aria-label="Video controls">
          <button id="toggle" class="toggle" aria-pressed="true" aria-label="Pause" title="Pause">
            <svg viewBox="0 0 24 24" aria-hidden="true"><path d="M6 4h4v16H6zM14 4h4v16h-4z"/></svg>
          </button>
          <div class="timeline" id="timeline">
            <label class="sr-only" for="scrubber">Timeline</label>
            <input id="scrubber" type="range" min="0" max="0" step="any" value="0" />
            <div id="pips" class="pips" aria-hidden="true"></div>
          </div>
        </div>
      </div>
    `;

    this._video    = this.shadowRoot.getElementById('vid');
    this._slider   = this.shadowRoot.getElementById('scrubber');
    this._pips     = this.shadowRoot.getElementById('pips');
    this._toggle   = this.shadowRoot.getElementById('toggle');
    this._timeline = this.shadowRoot.getElementById('timeline');

    this._bind();
  }

  _bind() {
    const video  = this._video;
    const slider = this._slider;
    const pips   = this._pips;
    const years  = this._years;

    const setToggleIcon = (isPlaying) => {
      this._toggle.setAttribute('aria-pressed', isPlaying ? 'true' : 'false');
      this._toggle.setAttribute('aria-label', isPlaying ? 'Pause' : 'Play');
      this._toggle.title = isPlaying ? 'Pause' : 'Play';
      this._toggle.innerHTML = isPlaying
        ? '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M6 4h4v16H6zM14 4h4v16h-4z"/></svg>'
        : '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M8 5v14l11-7z"/></svg>';
    };

    const updateSliderFill = () => {
      const max = parseFloat(slider.max) || 0;
      const val = parseFloat(slider.value) || 0;
      const pct = max > 0 ? (val / max) * 100 : 0;
      slider.style.setProperty('--progress', Math.max(0, Math.min(100, pct)) + '%');
    };

    // timing attrs
    const slen = parseFloat(this.getAttribute('state-length') || '1');
    const tlen = parseFloat(this.getAttribute('transition-length') || '4');
    const fps  = parseFloat(this.getAttribute('fps') || '60');
    const nfrm = parseFloat(this.getAttribute('nframes') || '900');
    const ep   = parseFloat(this.getAttribute('end-pause') || '0');
    const contentEndFromFrames = (nfrm > 0 && fps > 0 && ep >= 0) ? (nfrm - ep) / fps : null;

    let contentEnd = null;

    // helper: clamped fast seek
    const seekClamped = (t) => {
      const end = (contentEnd ?? video.duration) || 0;
      const tt = Math.max(0, Math.min(t, end));
      if (typeof video.fastSeek === 'function') video.fastSeek(tt);
      else video.currentTime = tt;
      slider.value = String(tt);
      updateSliderFill();
    };

    // loop control
    let loopGuard = false;
    const frameEps = 0.5 / (fps > 0 ? fps : 60);

    const maybeLoop = (mediaTime) => {
      const end = (contentEnd ?? video.duration) || 0;
      if (!end || video.paused || this._isScrubbing) return;
      if (mediaTime >= end - frameEps) {
        if (!loopGuard) {
          loopGuard = true;
          seekClamped(0);
          video.play().catch(()=>{});
        }
      } else {
        loopGuard = false;
      }
    };

    // Decorative pips
    const buildPips = (duration) => {
      pips.innerHTML = '';
      if (years.length <= 1 || !isFinite(duration) || duration <= 0) return;
      const N = years.length;
      const content = contentEndFromFrames ?? duration;
      const totalUnits = (N * slen) + ((N - 1) * tlen);
      const unitSec = content / totalUnits;
      years.forEach((_, idx) => {
        const t = idx * (slen + tlen) * unitSec;
        const leftPct = (t / duration) * 100;
        const holder = document.createElement('div');
        holder.className = 'pip';
        holder.style.left = leftPct + '%';
        holder.setAttribute('aria-hidden','true');
        pips.appendChild(holder);
      });
    };

    video.addEventListener('loadedmetadata', () => {
      const duration = video.duration || 0;
      contentEnd = (nfrm > 0 && fps > 0) ? Math.min(duration, (nfrm - ep) / fps) : duration;

      slider.max = String(contentEnd);
      slider.value = '0';
      updateSliderFill();
      buildPips(duration);
    });

    const updateFromMediaTime = (t) => {
      const end = (contentEnd ?? video.duration) || 0;
      const shown = Math.min(t, end);
      slider.value = String(shown);
      updateSliderFill();
    };

    // timeupdate + loop
    video.addEventListener('timeupdate', () => {
      maybeLoop(video.currentTime);
      if (!this._isScrubbing) updateFromMediaTime(video.currentTime);
    });

    video.addEventListener('play',  () => { this._autoplayOn = true;  setToggleIcon(true);  });
    video.addEventListener('pause', () => { this._autoplayOn = false; setToggleIcon(false); });

    /* --------- HOVER detection for independent pops ---------- */
    const HOVER_THRESHOLD_PX = 16; // distance from thumb center

    const updateHoverClasses = (e) => {
      if (this._isScrubbing) return; // don't hover-pop while scrubbing
      const rect = slider.getBoundingClientRect();
      const x = e.clientX - rect.left;
      const y = e.clientY - rect.top;
      const within = x >= 0 && x <= rect.width && y >= 0 && y <= rect.height;

      this._timeline.classList.remove('hover-thumb','hover-track');
      if (!within) return;

      const min = parseFloat(slider.min) || 0;
      const max = parseFloat(slider.max) || 0;
      const val = parseFloat(slider.value) || 0;
      const frac = (max > min) ? (val - min) / (max - min) : 0;
      const thumbX = frac * rect.width;
      const dist = Math.abs(x - thumbX);

      if (dist <= HOVER_THRESHOLD_PX) {
        this._timeline.classList.add('hover-thumb');   // pop the blue circle
      } else {
        this._timeline.classList.add('hover-track');   // pop only the track
      }
    };

    this._timeline.addEventListener('pointerenter', updateHoverClasses);
    this._timeline.addEventListener('pointermove',  updateHoverClasses);
    this._timeline.addEventListener('pointerleave', () => {
      this._timeline.classList.remove('hover-thumb','hover-track');
    });
    /* --------------------------------------------------------- */

    // Scrub pop effect
    const startScrub = () => {
      this._isScrubbing = true;
      this._timeline.classList.remove('hover-thumb','hover-track');
      this._timeline.classList.add('scrubbing');
      if (!video.paused) video.pause();
      this._autoplayOn = false;
      setToggleIcon(false);
    };
    const endScrub = () => {
      this._isScrubbing = false;
      this._timeline.classList.remove('scrubbing');
    };

    slider.addEventListener('pointerdown', startScrub);
    slider.addEventListener('pointerup',   endScrub);
    slider.addEventListener('pointercancel', endScrub);
    slider.addEventListener('keydown', (e) => {
      if (['ArrowLeft','ArrowRight','Home','End','PageUp','PageDown'].includes(e.key)) startScrub();
    });

    // Clamp + fast seek on input
    slider.addEventListener('input', () => {
      let t = parseFloat(slider.value);
      if (!Number.isFinite(t)) return;
      seekClamped(t);
    });

    // Toggle play/pause (button + video click)
    this._toggle.addEventListener('click', () => { if (!video.paused) video.pause(); else video.play().catch(()=>{}); });
    video.addEventListener('click', () => { if (video.paused) video.play(); else video.pause(); });

    // High-frequency syncing (rVFC preferred)
    let rafId = null, cancelVFC = null;
    const startRAF = () => {
      if (rafId) return;
      const tick = () => {
        maybeLoop(video.currentTime);
        if (!this._isScrubbing) updateFromMediaTime(video.currentTime);
        if (!video.paused && !video.ended) rafId = requestAnimationFrame(tick); else rafId = null;
      };
      rafId = requestAnimationFrame(tick);
    };
    const stopRAF = () => { if (rafId) { cancelAnimationFrame(rafId); rafId = null; } };

    const startVFC = () => {
      if (!('requestVideoFrameCallback' in HTMLVideoElement.prototype)) { startRAF(); return; }
      if (cancelVFC) return;
      const onFrame = (_now, meta) => {
        maybeLoop(meta.mediaTime);
        if (!this._isScrubbing) updateFromMediaTime(meta.mediaTime);
        if (!video.paused && !video.ended) vfcId = video.requestVideoFrameCallback(onFrame);
      };
      let vfcId = video.requestVideoFrameCallback(onFrame);
      cancelVFC = () => { if (vfcId) { video.cancelVideoFrameCallback?.(vfcId); cancelVFC = null; } };
    };

    video.addEventListener('play',  () => { stopRAF(); cancelVFC?.(); startVFC(); setToggleIcon(true);  });
    video.addEventListener('pause', () => { stopRAF(); cancelVFC?.(); setToggleIcon(false); });
    video.addEventListener('ended', () => { stopRAF(); cancelVFC?.(); });
  }
}

if (!customElements.get('year-video-slider')) {
  customElements.define('year-video-slider', YearVideoSlider);
}
